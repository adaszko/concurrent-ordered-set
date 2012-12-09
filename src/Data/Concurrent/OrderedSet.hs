module Data.Concurrent.OrderedSet (
  OrderedSet,
  empty,
  fromList,
  toList,
  insert,
  contains,
  delete
) where


-- TODO: Try unboxed types
-- TODO: Potential false-sharing in nodes' arrays
-- TODO: Use of lists in place of arrays would free the user from having to
--       specify maximal capacity and would possibly decrease false sharing


import Data.List (replicate, sortBy)
import Data.Array.IO
import Data.IORef
import Control.Monad
import System.Random (randomRIO)
import Control.Concurrent.MarkableIORef


type OrderedSetIORef a = IORef (OrderedSet a)
type NextArray a = IOArray Int (MarkableIORef (OrderedSet a))
type NodeArray a = IOArray Int (OrderedSet a)


data OrderedSet a
  = Head
    { maxLevel :: Int
    , next :: NextArray a
    }
  | Node
    { value :: a
    , next :: NextArray a
    , topLevel :: Int
    }
  | Tail


bottomLevel :: Int
bottomLevel = 1


newNextArray :: Int -> [OrderedSet a] -> IO (NextArray a)
newNextArray topLevel succs = do
  refs <- mapM newIORef succs
  markableRefs <- mapM (\ref -> newMarkableRef ref False) $ take topLevel refs
  newListArray (bottomLevel, topLevel) markableRefs


newNode :: a -> Int -> NodeArray a -> IO (OrderedSet a)
newNode value topLevel succs = do
  elems <- getElems succs
  nextArray <- newNextArray topLevel elems
  return Node { value = value, next = nextArray, topLevel = topLevel }


empty :: Int -> IO (OrderedSet a)
empty maxLevel = do
  skipPtrs <- newNextArray maxLevel $ replicate maxLevel Tail
  return Head { maxLevel = maxLevel, next = skipPtrs }


fromList :: Ord a => Int -> [a] -> IO (OrderedSet a)
fromList maxLevel contents = do
  list <- empty maxLevel
  mapM_ (\elem -> insert elem list) $ sortBy (flip compare) contents
  return list


toList :: OrderedSet a -> IO [a]
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


{- Returns:

1) elem found: (markableRef to node containing elem, successor of node
containing elem)

2) elem not found: (markableRef to node containing smallest value greater than
elem or Tail, node from first element of this pair)

-}
find :: Ord a => a -> OrderedSet a -> IO (NextArray a, OrderedSetIORef a, NodeArray a)
find elem head @ Head { maxLevel = maxLevel, next = firstMarkableRefs } = do
  currMRefs <- newArray_ (bottomLevel, maxLevel) :: IO (NextArray a)
  succs <- newArray_ (bottomLevel, maxLevel) :: Ord a => IO (NodeArray a)
  snapshotRef <- newIORef undefined
  go maxLevel firstMarkableRefs (currMRefs, snapshotRef, succs)
  where
    go 0 _ result = return result
    go level currMarkableRefs result @ (currMRefs, snapshotRef, succs) = do
      currMarkableRef <- readArray currMarkableRefs level
      snapshotRef <- readMarkableRef currMarkableRef
      curr <- readIORef snapshotRef
      marked <- isMarked currMarkableRef
      case curr of
        Tail -> do
          writeArray currMRefs level currMarkableRef
          writeArray succs level curr
          go (level - 1) currMarkableRefs (currMRefs, snapshotRef, succs)
        Node { value = val, next = succMarkableRefs } -> do
          succMarkableRef <- readArray succMarkableRefs level
          succRef <- readMarkableRef succMarkableRef
          if marked
            then do success <- compareAndSet currMarkableRef snapshotRef succRef True False
                    if success
                      then go level currMarkableRefs (currMRefs, snapshotRef, succs)
                      else find elem head
            else case val `compare` elem of
                   GT -> do writeArray currMRefs level currMarkableRef
                            if level > 1
                              then do succ <- readIORef succRef
                                      writeArray succs level succ
                                      go (level - 1) currMarkableRefs (currMRefs, snapshotRef, succs)
                              else do writeArray succs level curr
                                      return (currMRefs, snapshotRef, succs)

                   LT -> go level succMarkableRefs (currMRefs, snapshotRef, succs)

                   -- Even when the node with requested element is found, we
                   -- proceed to lower levels in order to fill up arrays of
                   -- predecessors and successors
                   EQ -> do writeArray currMRefs level currMarkableRef
                            succ <- readIORef succRef
                            writeArray succs level succ
                            go (level - 1) currMarkableRefs (currMRefs, snapshotRef, succs)


flipCoin :: IO Bool
flipCoin = randomRIO (False, True)


randomLevel :: Int -> IO Int
randomLevel maxLevel = do
  flips <- replicateM (maxLevel - 1) flipCoin
  let tails = takeWhile id (True : flips)
  return $ length tails


insert :: Ord a => a -> OrderedSet a -> IO Bool
insert elem head @ Head { maxLevel = maxLevel } = do
  (currMRefs, snapshotRef, succs) <- find elem head
  currMarkableRef <- readArray currMRefs bottomLevel
  curr <- readIORef snapshotRef
  case curr of
    Tail -> do
      updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs)
    Node { value = val } -> do
      -- INVARIANT curr contains smallest val in list such that elem <= val
      if val == elem
        then return False
        else updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs)

  where

    makeNode curr succs = do
      topLevel <- randomLevel maxLevel
      writeArray succs bottomLevel curr
      newNode elem topLevel succs

    updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs) = do
      newNode @ Node { topLevel = topLevel } <- makeNode curr succs
      newNodeRef <- newIORef newNode
      success <- compareAndSet currMarkableRef snapshotRef newNodeRef False False
      if not success
        then insert elem head
        else updateUpperLevels (bottomLevel+1) topLevel (currMRefs, snapshotRef, succs) newNode

    updateUpperLevels level topLevel (currMRefs, snapshotRef, succs) newNode
      | level > topLevel = return True
      | otherwise        = do
        currMarkableRef <- readArray currMRefs level
        currRef <- readMarkableRef currMarkableRef
        newNodeRef <- newIORef newNode
        success <- compareAndSet currMarkableRef currRef newNodeRef False False
        if success
          then updateUpperLevels (level + 1) topLevel (currMRefs, snapshotRef, succs) newNode
          else do pos <- find elem head
                  updateUpperLevels level topLevel pos newNode



delete :: Ord a => a -> OrderedSet a -> IO Bool
delete elem head = do
  (currMRefs, snapshotRef, succs) <- find elem head
  currMarkableRef <- readArray currMRefs bottomLevel
  curr <- readIORef snapshotRef
  case curr of
    Tail -> return False
    Node { value = val, topLevel = topLevel } -> do
      if val == elem
        then markUpperLevels topLevel currMRefs
        else return False

  where

    markUpperLevels level currMRefs
      | level == bottomLevel = markBottomLevel currMRefs
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
        then do find elem head -- physically remove marked nodes
                return True
        else if marked
             then return False
             else markBottomLevel currMRefs



contains :: Ord a => a -> OrderedSet a -> IO Bool
contains elem Head { maxLevel = maxLevel, next = firstMarkableRefs } = do
  firstMarkableRef <- readArray firstMarkableRefs maxLevel
  go maxLevel firstMarkableRefs
  where
    go 0 _ = return False
    go level currMarkableRefs = do
      currMarkableRef <- readArray currMarkableRefs level
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Tail -> go (level - 1) currMarkableRefs
        Node { value = val, next = succMarkableRefs } -> do
          marked <- isMarked currMarkableRef
          if marked
            then go level succMarkableRefs
            else case val `compare` elem of
                   GT -> go (level - 1) currMarkableRefs
                   EQ -> return True
                   LT -> go level succMarkableRefs
