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
import qualified Data.Vector.Mutable as V
import Control.Monad
import System.Random (randomRIO)
import Control.Concurrent.MarkableIORef


type MarkableRefArray a = V.IOVector (MarkableIORef (OrderedSet a))
type RefArray a = V.IOVector (IORef (OrderedSet a))


data OrderedSet a
  = Head
    { numLevels :: Int
    , next     :: MarkableRefArray a
    }
  | Node
    { key  :: a
    , next :: MarkableRefArray a
    }
  | Tail


bottomLevel :: Int
bottomLevel = 0


flipCoin :: IO Bool
flipCoin = randomRIO (False, True)


randomLevel :: Int -> IO Int
randomLevel numLevels = do
  flips <- replicateM (numLevels - 1) flipCoin
  let tails = takeWhile id (True : flips)
  return $ length tails


-- | The Int parameter specifies maximal height of nodes.
empty :: Int -> IO (OrderedSet a)
empty numLevels = do
  tailRef <- newIORef Tail
  nextRefs <- V.replicateM numLevels $ newMarkableRef tailRef False
  return Head { numLevels = numLevels, next = nextRefs }



-- | Calls insert repeatedly.  The worst complexity is exhibited when input
-- list is sorted in increasing order.
fromList :: Ord a => Int -> [a] -> IO (OrderedSet a)
fromList numLevels contents = do
  list <- empty numLevels
  mapM_ (flip insert list) contents
  return list


-- | /Wait-free/.
toList :: OrderedSet a -> IO [a]
toList Head { next = mrefs } = go mrefs

  where

  go mrefs = do
    curr <- V.read mrefs bottomLevel >>= readMarkableRef >>= readIORef
    case curr of
      Tail -> return []
      Node { key = key, next = succMRefs } -> do
        rest <- go succMRefs
        return $ key : rest


-- | /Lock-free/. 
find :: Ord a => a -> OrderedSet a -> IO (Bool, MarkableRefArray a, RefArray a)
find elem head @ Head { numLevels = numLevels, next = mrefs } = do
  currMRefs <- V.new numLevels
  currRefs <- V.new numLevels
  go (numLevels - 1) mrefs currMRefs currRefs

  where

  go level mrefs currMRefs currRefs 
    | level >= bottomLevel = do
      currMarkableRef <- V.read mrefs level
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Node { key = key, next = succMRefs } -> do
          (succRef, marked) <- V.read succMRefs level >>= readMarkableRefMark
          if marked
            then do snip <- compareAndSet currMarkableRef currRef succRef False False
                    if snip
                      then go level mrefs currMRefs currRefs
                      else find elem head
            else if key < elem
              then go level succMRefs currMRefs currRefs
              else do V.write currMRefs level currMarkableRef
                      V.write currRefs level currRef
                      go (level - 1) mrefs currMRefs currRefs
        Tail -> do
          V.write currMRefs level currMarkableRef
          V.write currRefs level currRef
          go (level - 1) mrefs currMRefs currRefs
    | otherwise = do
      curr <- V.read currRefs bottomLevel >>= readIORef
      case curr of
        Tail -> return (False, currMRefs, currRefs)
        Node { key = key } -> return (key == elem, currMRefs, currRefs)


-- | /Lock-free/.
insert :: Ord a => a -> OrderedSet a -> IO Bool
insert elem head @ Head { numLevels = numLevels } = do
  (found, currMRefs, currRefs) <- find elem head
  if found
    then return False
    else updateBottomLevel currMRefs currRefs

  where

  makeNodeRef :: a -> Int -> RefArray a -> IO (IORef (OrderedSet a))
  makeNodeRef key height refs = do
    nextArray <- V.new height
    mapM_ (\i -> do
      elem <- V.read refs i
      mref <- newMarkableRef elem False
      V.write nextArray i mref) [0 .. height - 1]
    newIORef Node { key = key, next = nextArray }

  updateBottomLevel currMRefs currRefs = do
    height <- randomLevel numLevels
    newNodeRef <- makeNodeRef elem height currRefs
    currMarkableRef <- V.read currMRefs bottomLevel
    succRef <- V.read currRefs bottomLevel
    success <- compareAndSet currMarkableRef succRef newNodeRef False False
    if success
      then updateUpperLevels (bottomLevel+1) height newNodeRef currMRefs currRefs
      else insert elem head

  updateUpperLevels level height newNodeRef currMRefs currRefs
    | level < height = do
        currMarkableRef <- V.read currMRefs level
        succRef <- V.read currRefs level
        success <- compareAndSet currMarkableRef succRef newNodeRef False False
        if success
          then updateUpperLevels (level + 1) height newNodeRef currMRefs currRefs
          else insert elem head
    | otherwise = return True


-- | /Wait-free/.
contains :: Ord a => a -> OrderedSet a -> IO Bool
contains elem head @ Head { numLevels = numLevels, next = mrefs } = go (numLevels - 1) mrefs

  where

  go level mrefs 
    | level >= bottomLevel = do
      curr <- V.read mrefs level >>= readMarkableRef >>= readIORef
      case curr of
        Node { key = key, next = succMRefs } -> do
          (succRef, marked) <- V.read succMRefs level >>= readMarkableRefMark
          if marked
            then go level succMRefs
            else case key `compare` elem of
              LT -> go level succMRefs
              EQ -> return True
              GT -> go (level - 1) mrefs
        Tail -> go (level - 1) mrefs
    | otherwise = return False


-- | /Lock-free/.
delete :: Ord a => a -> OrderedSet a -> IO Bool
delete elem head = do
  (found, currMRefs, currRefs) <- find elem head
  if not found
    then return False
    else do subject <- V.read currRefs bottomLevel >>= readIORef
            let height = V.length $ next subject
            updateUpperLevels (height - 1) subject

  where

  updateUpperLevels level subject
    | level == bottomLevel = updateBottomLevel subject
    | otherwise = do
      succMarkableRef <- V.read (next subject) level
      (succ, marked) <- readMarkableRefMark succMarkableRef
      success <- attemptMark succMarkableRef succ True
      if success
        then updateUpperLevels (level - 1) subject
        else updateUpperLevels level subject

  updateBottomLevel subject = do
    succMarkableRef <- V.read (next subject) bottomLevel
    succRef <- readMarkableRef succMarkableRef
    iMarkedIt <- compareAndSet succMarkableRef succRef succRef False True
    (succRef, marked) <- readMarkableRefMark succMarkableRef
    if iMarkedIt
      then find elem head >> return True
      else if marked
        then return False
        else updateBottomLevel subject
