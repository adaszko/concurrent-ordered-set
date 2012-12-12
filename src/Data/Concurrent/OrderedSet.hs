{-|

   This implementation of sets is based on /skip lists/.  Atomicity is
   achieved thanks to @atomicModifyIORef@.

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
    , next     :: NextArray a
    }
  | Node
    { value :: a
    , next  :: NextArray a
    }
  | Tail


bottomLevel :: Int
bottomLevel = 1


newEmptyNextArray :: Int -> IO (NextArray a)
newEmptyNextArray topLevel = newArray_ (bottomLevel, topLevel)


newEmptyNodeArray :: Int -> IO (NodeArray a)
newEmptyNodeArray topLevel = newArray_ (bottomLevel, topLevel)


newNextArray :: Int -> [OrderedSet a] -> IO (NextArray a)
newNextArray topLevel succs = do
  refs <- mapM newIORef succs
  markableRefs <- mapM (flip newMarkableRef False) $ take topLevel refs
  newListArray (bottomLevel, topLevel) markableRefs


newNode :: a -> Int -> NodeArray a -> IO (OrderedSet a)
newNode value topLevel succs = do
  elems <- getElems succs
  nextArray <- newNextArray topLevel elems
  return Node { value = value, next = nextArray }


empty :: Int -> IO (OrderedSet a)
empty maxLevel = do
  skipPtrs <- newNextArray maxLevel $ replicate maxLevel Tail
  return Head { maxLevel = maxLevel, next = skipPtrs }


fromList :: Ord a => Int -> [a] -> IO (OrderedSet a)
fromList maxLevel contents = do
  list <- empty maxLevel
  mapM_ (flip insert list) $ sortBy (flip compare) contents
  return list


-- | Wait-free
toList :: OrderedSet a -> IO [a]
toList Head { next = firstMarkableRefs } = go firstMarkableRefs
  where
    go currMarkableRefs = do
      (bottomLevel, _) <- getBounds currMarkableRefs
      currMarkableRef <- readArray currMarkableRefs bottomLevel
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Tail ->
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
  currMRefs <- newEmptyNextArray maxLevel
  succs <- newEmptyNodeArray maxLevel
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


-- | Lock-free
insert :: Ord a => a -> OrderedSet a -> IO Bool
insert elem head @ Head { maxLevel = maxLevel } = do
  (currMRefs, snapshotRef, succs) <- find elem head
  (bottomLevel, _) <- getBounds currMRefs
  currMarkableRef <- readArray currMRefs bottomLevel
  curr <- readIORef snapshotRef
  case curr of
    Tail ->
      updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs)
    Node { value = val } ->
      -- INVARIANT curr contains smallest val in list such that elem <= val
      if val == elem
        then return False
        else updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs)

  where

    makeNode curr succs = do
      topLevel <- randomLevel maxLevel
      (bottomLevel, _) <- getBounds succs
      writeArray succs bottomLevel curr
      newNode elem topLevel succs

    updateBottomLevel currMarkableRef curr (currMRefs, snapshotRef, succs) = do
      newNode <- makeNode curr succs
      newNodeRef <- newIORef newNode
      success <- compareAndSet currMarkableRef snapshotRef newNodeRef False False
      if not success
        then insert elem head
        else do elems <- getElems $ next newNode
                updateUpperLevels (drop 1 elems) newNode

    updateUpperLevels (currMarkableRef : rest) newNode = do
      currRef <- readMarkableRef currMarkableRef
      newNodeRef <- newIORef newNode
      success <- compareAndSet currMarkableRef currRef newNodeRef False False
      if success
        then updateUpperLevels rest newNode
        else do (newCurrMRefs, _, _) <- find elem head
                elems <- getElems newCurrMRefs
                updateUpperLevels elems newNode
    updateUpperLevels [] _ = return True



-- | Lock-free
delete :: Ord a => a -> OrderedSet a -> IO Bool
delete elem head = do
  (currMRefs, snapshotRef, succs) <- find elem head
  (bottomLevel, _) <- getBounds currMRefs
  currMarkableRef <- readArray currMRefs bottomLevel
  curr <- readIORef snapshotRef
  case curr of
    Tail -> return False
    Node { value = val } ->
      if val == elem
        then do elems <- getElems currMRefs
                markUpperLevels $ reverse elems
        else return False

  where

    markUpperLevels [currMarkableRef] = markBottomLevel currMarkableRef
    markUpperLevels curr @ (currMarkableRef : rest) = do
      currRef <- readMarkableRef currMarkableRef
      success <- attemptMark currMarkableRef currRef True
      if success
        then markUpperLevels rest
        else markUpperLevels curr

    markBottomLevel currMarkableRef = do
      currRef <- readMarkableRef currMarkableRef
      iMarkedIt <- compareAndSet currMarkableRef currRef currRef False True
      marked <- isMarked currMarkableRef
      if iMarkedIt
        then do find elem head -- physically remove marked nodes
                return True
        else if marked
             then return False
             else markBottomLevel currMarkableRef


-- | Wait-free
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
