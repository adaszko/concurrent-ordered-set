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


import Prelude hiding (head, elem)
import Data.IORef
import qualified Data.Vector.Mutable as V
import qualified System.Random as R
import Control.Concurrent.MarkableIORef


type MarkableRefArray a = V.IOVector (MarkableIORef (OrderedSet a))
type RefArray a = V.IOVector (IORef (OrderedSet a))


data OrderedSet a
  = Head
    { randomGen :: IORef R.StdGen
    , maxLevels :: Int
    , next     :: MarkableRefArray a
    }
  | Node
    { key  :: a
    , next :: MarkableRefArray a
    }
  | Tail


bottomLevel :: Int
bottomLevel = 0


iter :: (g -> (a, g)) -> Int -> g -> (g, [a])
iter _ 0 g = (g, [])
iter f n g = (g'', x:xs)
  where
    (x, g') = f g
    (g'', xs) = iter f (n-1) g'


randomLevel :: R.StdGen -> Int -> (R.StdGen, Int)
randomLevel gen numLevels = (gen', length tails)
  where
    (gen', flips) = iter (R.randomR (False, True)) (numLevels - 1) gen
    tails = takeWhile id (True : flips)


-- | The Int parameter specifies maximal height of nodes.
empty :: R.StdGen -> Int -> IO (OrderedSet a)
empty gen numLevels = do
  tailRef <- newIORef Tail
  nextRefs <- V.replicateM numLevels $ newMarkableRef tailRef False
  genRef <- newIORef gen
  return Head { randomGen = genRef, maxLevels = numLevels, next = nextRefs }



-- | Calls insert repeatedly.  The worst complexity is exhibited when input
-- list is sorted in increasing order.
fromList :: Ord a => R.StdGen -> Int -> [a] -> IO (OrderedSet a)
fromList gen numLevels contents = do
  list <- empty gen numLevels
  mapM_ (flip insert list) contents
  return list


-- | /Wait-free/.
toList :: OrderedSet a -> IO [a]
toList Head { next = markableRefs } = go markableRefs

  where

  go mrefs = do
    curr <- V.read mrefs bottomLevel >>= readMarkableRef >>= readIORef
    case curr of
      Tail -> return []
      Node { key = val, next = succMRefs } -> do
        rest <- go succMRefs
        return $ val : rest
      Head _ _ _ -> undefined

toList (Node _ _) = undefined
toList Tail = undefined


-- | /Lock-free/.
find :: Ord a => a -> OrderedSet a -> IO (Bool, MarkableRefArray a, RefArray a)
find elem head @ Head { maxLevels = numLevels, next = markableRefs } = do
  currMRefs <- V.new numLevels
  currRefs <- V.new numLevels
  go (numLevels - 1) markableRefs currMRefs currRefs

  where

  go level mrefs currMRefs currRefs
    | level >= bottomLevel = do
      currMarkableRef <- V.read mrefs level
      currRef <- readMarkableRef currMarkableRef
      curr <- readIORef currRef
      case curr of
        Node { key = val, next = succMRefs } -> do
          (succRef, marked) <- V.read succMRefs level >>= readMarkableRefMark
          if marked
            then do snip <- compareAndSet currMarkableRef currRef succRef False False
                    if snip
                      then go level mrefs currMRefs currRefs
                      else find elem head
            else if val < elem
              then go level succMRefs currMRefs currRefs
              else do V.write currMRefs level currMarkableRef
                      V.write currRefs level currRef
                      go (level - 1) mrefs currMRefs currRefs
        Tail -> do
          V.write currMRefs level currMarkableRef
          V.write currRefs level currRef
          go (level - 1) mrefs currMRefs currRefs
        Head _ _ _ -> undefined
    | otherwise = do
      curr <- V.read currRefs bottomLevel >>= readIORef
      case curr of
        Tail -> return (False, currMRefs, currRefs)
        Node { key = val } -> return (val == elem, currMRefs, currRefs)
        Head _ _ _ -> undefined
find _ _ = undefined


-- | /Lock-free/.
insert :: Ord a => a -> OrderedSet a -> IO Bool
insert elem head @ Head { randomGen = genRef, maxLevels = numLevels } = do
  (found, currMRefs, currRefs) <- find elem head
  if found
    then return False
    else updateBottomLevel currMRefs currRefs

  where

  makeNodeRef :: a -> Int -> RefArray a -> IO (IORef (OrderedSet a))
  makeNodeRef val height refs = do
    nextArray <- V.new height
    mapM_ (\i -> do
      ref <- V.read refs i
      mref <- newMarkableRef ref False
      V.write nextArray i mref) [0 .. height - 1]
    newIORef Node { key = val, next = nextArray }

  updateBottomLevel currMRefs currRefs = do
    rGen <- readIORef genRef
    let (rGen', height) = randomLevel rGen numLevels
    newNodeRef <- makeNodeRef elem height currRefs
    currMarkableRef <- V.read currMRefs bottomLevel
    succRef <- V.read currRefs bottomLevel
    success <- compareAndSet currMarkableRef succRef newNodeRef False False
    if success
      then updateUpperLevels rGen' (bottomLevel+1) height newNodeRef currMRefs currRefs
      else insert elem head

  updateUpperLevels rGen' level height newNodeRef currMRefs currRefs
    | level < height = do
        currMarkableRef <- V.read currMRefs level
        succRef <- V.read currRefs level
        success <- compareAndSet currMarkableRef succRef newNodeRef False False
        if success
          then updateUpperLevels rGen' (level + 1) height newNodeRef currMRefs currRefs
          else insert elem head
    | otherwise = do
      _ <- atomicModifyIORef genRef $ const (rGen', undefined)
      return True
insert _ _ = undefined


-- | /Wait-free/.
contains :: Ord a => a -> OrderedSet a -> IO Bool
contains elem Head { maxLevels = numLevels, next = markableRefs } = go (numLevels - 1) markableRefs

  where

  go level mrefs
    | level >= bottomLevel = do
      curr <- V.read mrefs level >>= readMarkableRef >>= readIORef
      case curr of
        Node { key = val, next = succMRefs } -> do
          (_, marked) <- V.read succMRefs level >>= readMarkableRefMark
          if marked
            then go level succMRefs
            else case val `compare` elem of
              LT -> go level succMRefs
              EQ -> return True
              GT -> go (level - 1) mrefs
        Tail -> go (level - 1) mrefs
        Head _ _ _ -> undefined
    | otherwise = return False
contains _ _ = undefined


-- | /Lock-free/.
delete :: Ord a => a -> OrderedSet a -> IO Bool
delete elem head = do
  (found, _, currRefs) <- find elem head
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
      (succRef, _) <- readMarkableRefMark succMarkableRef
      success <- attemptMark succMarkableRef succRef True
      if success
        then updateUpperLevels (level - 1) subject
        else updateUpperLevels level subject

  updateBottomLevel subject = do
    succMarkableRef <- V.read (next subject) bottomLevel
    succRef <- readMarkableRef succMarkableRef
    iMarkedIt <- compareAndSet succMarkableRef succRef succRef False True
    (_, marked) <- readMarkableRefMark succMarkableRef
    if iMarkedIt
      then find elem head >> return True
      else if marked
        then return False
        else updateBottomLevel subject
