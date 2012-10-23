{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.Concurrent.OrderedMap (
  OrderedMap,
  empty,
  fromList,
  toList,
  insert,
  contains,
  delete
  ) where


-- XXX: Remove (Show a) from contexts

-- TODO: Try unboxed types
-- TODO: Potential false-sharing in nodes' arrays
-- TODO: Use of lists insead of arrays would free the user from having to
--       specify maximal capacity and would potentially decrease false sharing


import Data.List (replicate, sortBy)
import Data.Array.IO
import Data.IORef
import Control.Monad
import System.Random (randomRIO)
import Control.Concurrent.MarkableIORef
import Debug.Trace -- XXX
import System.IO.Unsafe -- XXX


type NextArray a = IOArray Int (MarkableIORef (OrderedMap a))
type NodeArray a = IOArray Int (OrderedMap a)


--- XXX: Remove this
instance Show a => Show (MarkableIORef a) where
  show = show . unsafePerformIO . readIORef . unsafePerformIO . readMarkableRef

-- XXX: Remove this
instance Show a => Show (NextArray a) where
  show = show . unsafePerformIO . getElems

-- XXX: Remove this
instance Show a => Show (NodeArray a) where
  show = show . unsafePerformIO . getElems


data OrderedMap a
  = Head
    { maxLevel :: Int
    , next :: NextArray a
    }
  | Node
    { value :: a
    , next :: NextArray a
    , topLevel :: Int
    }
  | Tail deriving (Show) -- XXX: Remove this


bottomLevel :: Int
bottomLevel = 1


newNextArray :: Int -> [OrderedMap a] -> IO (NextArray a)
newNextArray topLevel succs = do
  refs <- mapM newIORef succs
  markableRefs <- mapM (\ref -> newMarkableRef ref False) $ take topLevel refs
  newListArray (bottomLevel, topLevel) markableRefs


newNode :: a -> Int -> NodeArray a -> IO (OrderedMap a)
newNode value topLevel succs = do
  elems <- getElems succs
  nextArray <- newNextArray topLevel elems
  return Node { value = value, next = nextArray, topLevel = topLevel }


empty :: Int -> IO (OrderedMap a)
empty maxLevel = do
  skipPtrs <- newNextArray maxLevel $ replicate maxLevel Tail
  return Head { maxLevel = maxLevel, next = skipPtrs }


fromList :: (Show a, Ord a) => Int -> [a] -> IO (OrderedMap a)
fromList maxLevel contents = do
  list <- empty maxLevel
  mapM_ (\elem -> insert elem list) $ sortBy (flip compare) contents
  return list


toList :: OrderedMap a -> IO [a]
toList Head { next = firstMarkableRefs } = go firstMarkableRefs
  where
    go currMarkableRefs = do
      currMarkableRef <- readArray currMarkableRefs bottomLevel
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Tail -> do
          return []
        Node { value = val, next = succMarkableRefs } -> do
          rest <- go succMarkableRefs
          return $ val : rest


find :: (Show a, Ord a) => a -> OrderedMap a -> IO (NextArray a, NodeArray a)
find elem head @ Head { maxLevel = maxLevel, next = firstMarkableRefs } = do
  currMRefs <- newArray_ (bottomLevel, maxLevel) :: IO (NextArray a)
  succs <- newArray_ (bottomLevel, maxLevel) :: Ord a => IO (NodeArray a)
  go maxLevel firstMarkableRefs (currMRefs, succs)
  where
    go 0 _ result = return result
    go level currMarkableRefs result @ (currMRefs, succs) = do
      currMarkableRef <- readArray currMarkableRefs level
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      print curr
      case curr of
        Tail -> do
          writeArray currMRefs level currMarkableRef
          writeArray succs level curr
          go (level - 1) currMarkableRefs (currMRefs, succs)
        Node { value = val, next = succMarkableRefs } -> do
          succMarkableRef <- readArray succMarkableRefs level
          succRef <- readMarkableRef succMarkableRef
          marked <- isMarked succMarkableRef
          if marked
            then do success <- compareAndSet currMarkableRef currRef succRef False False
                    if success
                      then go level currMarkableRefs result
                      else find elem head
            else do writeArray currMRefs level currMarkableRef
                    succ <- readIORef succRef
                    writeArray succs level succ
                    if val < elem
                      then go (level - 1) succMarkableRefs result
                      else return result


flipCoin :: IO Bool
flipCoin = randomRIO (False, True)


randomLevel :: Int -> IO Int
randomLevel maxLevel = do
  flips <- replicateM (maxLevel - 1) flipCoin
  let tails = takeWhile id (True : flips)
  return $ length tails


insert :: (Show a, Ord a) => a -> OrderedMap a -> IO Bool
insert elem head @ Head { maxLevel = maxLevel } = do
  (currMRefs, succs) <- find elem head
  currMarkableRef <- readArray currMRefs bottomLevel
  currRef <- readMarkableRef currMarkableRef
  curr <- readIORef currRef
  case curr of
    Tail -> do
      print "tail"
      updateBottomLevel currMarkableRef currRef (currMRefs, succs)
    Node { value = val } -> do
      print "jestem"
      if val == elem
        then return False
        else updateBottomLevel currMarkableRef currRef (currMRefs, succs)

  where

    makeNode succs = do
      topLevel <- randomLevel maxLevel
      newNode elem topLevel succs

    updateBottomLevel currMarkableRef currRef (currMRefs, succs) = do
      newNode @ Node { topLevel = topLevel } <- makeNode succs
      newNodeRef <- newIORef newNode
      success <- compareAndSet currMarkableRef currRef newNodeRef False False
      print $ "success: " ++ show success
      if not success
        then insert elem head
        else updateUpperLevels (bottomLevel+1) topLevel (currMRefs, succs) newNode

    updateUpperLevels level topLevel (currMRefs, succs) newNode
      | level > topLevel = return True
      | otherwise        = do
        currMarkableRef <- readArray currMRefs level
        currRef <- readMarkableRef currMarkableRef
        newNodeRef <- newIORef newNode
        success <- compareAndSet currMarkableRef currRef newNodeRef False False
        if success
          then updateUpperLevels (level + 1) topLevel (currMRefs, succs) newNode
          else do pos <- find elem head
                  updateUpperLevels level topLevel pos newNode



delete :: (Show a, Ord a) => a -> OrderedMap a -> IO Bool
delete elem head = do
  (currMRefs, succs) <- find elem head
  currMarkableRef <- readArray currMRefs bottomLevel
  currRef <- readMarkableRef currMarkableRef
  curr <- readIORef currRef
  case curr of
    Tail -> return False
    Node { value = val, topLevel = topLevel } -> do
      if val == elem
        then do markUpperLevels topLevel currMRefs
                markBottomLevel currMRefs
        else return False

  where

    markUpperLevels level currMRefs
      | level == 0 = return ()
      | otherwise  = do
        currMarkableRef <- readArray currMRefs level
        currRef <- readMarkableRef currMarkableRef
        success <- attemptMark currMarkableRef currRef True
        if success
          then markUpperLevels (level - 1) currMRefs
          else markUpperLevels level currMRefs

    markBottomLevel currMRefs = do
      currMarkableRef <- readArray currMRefs bottomLevel
      currRef <- readMarkableRef currMarkableRef
      iMarkedIt <- compareAndSet currMarkableRef currRef currRef False True
      marked <- isMarked currMarkableRef
      if iMarkedIt
        then do find elem head
                return True
        else if marked
             then return False
             else markBottomLevel currMRefs



contains :: Ord a => a -> OrderedMap a -> IO Bool
contains elem Head { maxLevel = maxLevel, next = firstMarkableRefs } = do
  firstMarkableRef <- readArray firstMarkableRefs maxLevel
  go maxLevel firstMarkableRefs
  where
    go 0 _ = return False
    go level currMarkableRefs = do
      currMarkableRef <- readArray currMarkableRefs level
      marked <- isMarked currMarkableRef
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Tail -> go (level - 1) currMarkableRefs
        Node { value = val, next = succMarkableRefs } -> do
          marked <- isMarked currMarkableRef
          if marked
            then go level succMarkableRefs
            else if val < elem
                 then go level succMarkableRefs
                 else if val == elem
                      then return True
                      else go (level - 1) succMarkableRefs
