{-|

   Concurrent implementation of sets is based on /skip lists/.  Atomicity
   is achieved indirectly thanks to @atomicModifyIORef@.

-}

module Data.Concurrent.OrderedSet (
  OrderedSet,
  empty,
  fromList,
  toList,
  insert,
  contains,
  delete
) where


import Data.IORef
import Data.List (sortBy)
import Data.Array.IO
import Control.Monad
import System.Random (randomRIO)
import Control.Concurrent.MarkableIORef


type MarkableRefArray a = IOArray Int (MarkableIORef (OrderedSet a))
type RefArray a = IOArray Int (IORef (OrderedSet a))


data OrderedSet a
  = Head
    { maxLevel :: Int
    , next     :: MarkableRefArray a
    }
  | Node
    { key  :: a
    , next :: MarkableRefArray a
    }
  | Tail


bottomLevel :: Int
bottomLevel = 1


flipCoin :: IO Bool
flipCoin = randomRIO (False, True)


randomLevel :: Int -> IO Int
randomLevel maxLevel = do
  flips <- replicateM (maxLevel - 1) flipCoin
  let tails = takeWhile id (True : flips)
  return $ length tails


-- | The Int parameter specifies maximal height of nodes.
empty :: Int -> IO (OrderedSet a)
empty maxLevel = do
  tailRef <- newIORef Tail
  mrefs <- mapM (flip newMarkableRef False) $ replicate maxLevel tailRef
  nextRefs <- newListArray (bottomLevel, maxLevel) mrefs
  return Head { maxLevel = maxLevel, next = nextRefs }



-- | Calls insert repeatedly.  The worst complexity is exhibited when input
-- list is sorted in increasing order.
fromList :: Ord a => Int -> [a] -> IO (OrderedSet a)
fromList maxLevel contents = do
  list <- empty maxLevel
  mapM_ (flip insert list) contents
  return list



-- | /Wait-free/.
toList :: OrderedSet a -> IO [a]
toList Head { next = mrefs } = go mrefs

  where

  go mrefs = do
    curr <- readArray mrefs bottomLevel >>= readMarkableRef >>= readIORef
    case curr of
      Tail -> return []
      Node { key = key, next = succMRefs } -> do
        rest <- go succMRefs
        return $ key : rest


-- | /Lock-free/. 
find :: Ord a => a -> OrderedSet a -> IO (Bool, MarkableRefArray a, RefArray a)
find elem head @ Head { maxLevel = maxLevel, next = mrefs } = do
  currMRefs <- newArray_ (bottomLevel, maxLevel) :: IO (MarkableRefArray a)
  currRefs <- newArray_ (bottomLevel, maxLevel) :: IO (RefArray a)
  go maxLevel mrefs currMRefs currRefs

  where

  go 0 _ currMRefs currRefs = do
    curr <- readArray currRefs bottomLevel >>= readIORef
    case curr of
      Tail -> return (False, currMRefs, currRefs)
      Node { key = key } -> return (key == elem, currMRefs, currRefs)
  go level mrefs currMRefs currRefs = do
    currMarkableRef <- readArray mrefs level
    currRef <- readMarkableRef currMarkableRef
    curr <- readIORef currRef
    case curr of
      Node { key = key, next = succMRefs } -> do
        (succRef, marked) <- readArray succMRefs level >>= readMarkableRefMark
        if marked
          then do snip <- compareAndSet currMarkableRef currRef succRef False False
                  if snip
                    then go level mrefs currMRefs currRefs
                    else find elem head
          else if key < elem
            then go level succMRefs currMRefs currRefs
            else do writeArray currMRefs level currMarkableRef
                    writeArray currRefs level currRef
                    go (level - 1) mrefs currMRefs currRefs
      Tail -> do
        writeArray currMRefs level currMarkableRef
        writeArray currRefs level currRef
        go (level - 1) mrefs currMRefs currRefs



-- | /Lock-free/.
insert :: Ord a => a -> OrderedSet a -> IO Bool
insert elem head @ Head { maxLevel = maxLevel } = do
  (found, currMRefs, currRefs) <- find elem head
  if found
    then return False
    else updateBottomLevel currMRefs currRefs

  where

  makeNodeRef :: a -> Int -> RefArray a -> IO (IORef (OrderedSet a))
  makeNodeRef key topLevel refs = do
    elems <- getElems refs
    mrefs <- mapM (flip newMarkableRef False) elems
    nextArray <- newListArray (bottomLevel, topLevel) mrefs
    newIORef Node { key = key, next = nextArray }

  updateBottomLevel currMRefs currRefs = do
    topLevel <- randomLevel maxLevel
    newNodeRef <- makeNodeRef elem topLevel currRefs
    currMarkableRef <- readArray currMRefs bottomLevel
    succRef <- readArray currRefs bottomLevel
    success <- compareAndSet currMarkableRef succRef newNodeRef False False
    if success
      then updateUpperLevels (bottomLevel+1) topLevel newNodeRef currMRefs currRefs
      else insert elem head

  updateUpperLevels level topLevel newNodeRef currMRefs currRefs
    | level < topLevel = do
        currMarkableRef <- readArray currMRefs level
        succRef <- readArray currRefs level
        success <- compareAndSet currMarkableRef succRef newNodeRef False False
        if success
          then updateUpperLevels (level + 1) topLevel newNodeRef currMRefs currRefs
          else insert elem head
    | otherwise = return True



-- | /Wait-free/.
contains :: Ord a => a -> OrderedSet a -> IO Bool
contains elem head @ Head { maxLevel = maxLevel, next = mrefs } = go maxLevel mrefs

  where

  go 0 _ = return False
  go level mrefs = do
    curr <- readArray mrefs level >>= readMarkableRef >>= readIORef
    case curr of
      Node { key = key, next = succMRefs } -> do
        (succRef, marked) <- readArray succMRefs level >>= readMarkableRefMark
        if marked
          then go level succMRefs
          else case key `compare` elem of
            LT -> go level succMRefs
            EQ -> return True
            GT -> go (level - 1) mrefs
      Tail -> go (level - 1) mrefs



-- | /Lock-free/.
delete :: Ord a => a -> OrderedSet a -> IO Bool
delete elem head = do
  (found, currMRefs, currRefs) <- find elem head
  if not found
    then return False
    else do subject <- readArray currRefs bottomLevel >>= readIORef
            (_, topLevel) <- getBounds $ next subject
            updateUpperLevels topLevel subject

  where

  updateUpperLevels level subject
    | level == bottomLevel = updateBottomLevel subject
    | otherwise = do
      succMarkableRef <- readArray (next subject) level
      (succ, marked) <- readMarkableRefMark succMarkableRef
      success <- attemptMark succMarkableRef succ True
      if success
        then updateUpperLevels (level - 1) subject
        else updateUpperLevels level subject

  updateBottomLevel subject = do
    succMarkableRef <- readArray (next subject) bottomLevel
    succRef <- readMarkableRef succMarkableRef
    iMarkedIt <- compareAndSet succMarkableRef succRef succRef False True
    (succRef, marked) <- readMarkableRefMark succMarkableRef
    if iMarkedIt
      then find elem head >> return True
      else if marked
        then return False
        else updateBottomLevel subject
