import Criterion.Main
import Control.Concurrent
import Data.Concurrent.OrderedSet
import Control.Exception
import Control.Monad
import System.Random
import qualified Data.Set as S
import Control.Concurrent.MarkableIORef (casIORef)
import Data.IORef


height :: Int
height = 10


nElems :: Int
nElems = 2 ^ height


elemRange :: (Int, Int)
elemRange = (1, nElems)


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkIO (io `finally` putMVar mvar ())
  return mvar


genElems :: Int -> IO [[Int]]
genElems numThreads = replicateM numThreads $ genSubList $ nElems `div` numThreads
  where
    genSubList numElems = replicateM numElems $ randomRIO elemRange


destructiveInsert :: StdGen -> [[Int]] -> IO [Int]
destructiveInsert seed elements = do
  oset <- empty seed height
  mvars <- mapM (myForkIO . mapM_ (flip insert oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


destructiveContains :: StdGen -> [[Int]] -> IO [Bool]
destructiveContains seed elements = do
  oset <- fromList seed height $ concat elements
  results <- mapM (const $ newMVar []) elements
  threadsFinishMVars <- mapM (\(es, mv) -> myForkIO $ threadFn oset es mv) $ zip elements results
  mapM_ takeMVar threadsFinishMVars
  mapM takeMVar results >>= return . concat
    where
      threadFn oset es mv = mapM_ (syncContains oset mv) es
      syncContains oset mv e = do
        has <- contains e oset
        cur <- takeMVar mv
        putMVar mv $ has : cur


destructiveDelete :: StdGen -> [[Int]] -> IO [Int]
destructiveDelete seed elements = do
  oset <- fromList seed height $ concat elements
  mvars <- mapM (myForkIO . mapM_ (flip delete oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


pureSetOp :: S.Set Int -> (Int -> S.Set Int -> S.Set Int) -> [[Int]] -> IO [Int]
pureSetOp initial op elements = do
  sharedRef <- newIORef initial >>= newIORef
  threadsFinishMVars <- mapM (myForkIO . threadFn sharedRef) elements
  mapM_ takeMVar threadsFinishMVars -- make sure all threads completed their jobs
  readIORef sharedRef >>= readIORef >>= return . S.toList
    where
      threadFn sharedRef = mapM_ (loopCAS sharedRef)
      loopCAS sharedRef e = do
        curRootRef <- readIORef sharedRef
        cur <- readIORef curRootRef
        newRootRef <- newIORef $ op e cur
        success <- casIORef sharedRef curRootRef newRootRef
        unless success $ loopCAS sharedRef e


pureInsert :: [[Int]] -> IO [Int]
pureInsert = pureSetOp S.empty S.insert


pureContains :: [[Int]] -> IO [Bool]
pureContains elements = do
  sharedRef <- newIORef (S.fromList $ concat elements) >>= newIORef
  results <- mapM (const $ newMVar []) elements
  threadsFinishMVars <- mapM (\(es, mv) -> myForkIO $ threadFn sharedRef es mv) $ zip elements results
  mapM_ takeMVar threadsFinishMVars
  mapM takeMVar results >>= return . concat
    where
      threadFn sharedRef es mv = mapM_ (\e -> unwrapContains sharedRef e mv) es
      unwrapContains sharedRef e mv = do
        curRootRef <- readIORef sharedRef
        cur <- readIORef curRootRef
        r <- takeMVar mv
        putMVar mv $ S.member e cur : r


pureDelete :: [[Int]] -> IO [Int]
pureDelete elements = pureSetOp (S.fromList $ concat elements) S.delete elements


main :: IO ()
main = do
  let [e1, e2, e3, e4] = map genElems [1..4]
  mapM_ nfIO [e1, e2, e3, e4]

  let seed = mkStdGen 1792

  elems1 <- e1
  elems2 <- e2
  elems3 <- e3
  elems4 <- e4

  defaultMain
    [ bgroup "destructive 1"
      [ bench "insert" (nfIO $ destructiveInsert seed elems1)
      , bench "contains" (nfIO $ destructiveContains seed elems1)
      , bench "delete" (nfIO $ destructiveDelete seed elems1)
      ]
    , bgroup "destructive 2"
      [ bench "insert" (nfIO $ destructiveInsert seed elems2)
      , bench "contains" (nfIO $ destructiveContains seed elems2)
      , bench "delete" (nfIO $ destructiveDelete seed elems2)
      ]
    , bgroup "destructive 3"
      [ bench "insert" (nfIO $ destructiveInsert seed elems3)
      , bench "contains" (nfIO $ destructiveContains seed elems3)
      , bench "delete" (nfIO $ destructiveDelete seed elems3)
      ]
    , bgroup "destructive 4"
      [ bench "insert" (nfIO $ destructiveInsert seed elems4)
      , bench "contains" (nfIO $ destructiveContains seed elems4)
      , bench "delete" (nfIO $ destructiveDelete seed elems4)
      ]
    , bgroup "pure 1"
      [ bench "insert" (nfIO $ pureInsert elems2)
      , bench "contains" (nfIO $ pureContains elems2)
      , bench "delete" (nfIO $ pureDelete elems2)
      ]
    , bgroup "pure 2"
      [ bench "insert" (nfIO $ pureInsert elems2)
      , bench "contains" (nfIO $ pureContains elems2)
      , bench "delete" (nfIO $ pureDelete elems2)
      ]
    , bgroup "pure 3"
      [ bench "insert" (nfIO $ pureInsert elems3)
      , bench "contains" (nfIO $ pureContains elems3)
      , bench "delete" (nfIO $ pureDelete elems3)
      ]
    , bgroup "pure 4"
      [ bench "insert" (nfIO $ pureInsert elems4)
      , bench "contains" (nfIO $ pureContains elems4)
      , bench "delete" (nfIO $ pureDelete elems4)
      ]
    ]
