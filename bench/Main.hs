import Criterion.Main
import Control.Concurrent
import Data.Concurrent.OrderedSet
import Control.Exception
import Control.Monad
import System.Random
import qualified Data.Set as S


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


destructiveInsert :: StdGen -> [[Int]] -> IO ()
destructiveInsert seed elements = nfIO $ do
  oset <- empty seed height
  mvars <- mapM (myForkIO . mapM_ (flip insert oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


destructiveContains :: StdGen -> [[Int]] -> IO ()
destructiveContains seed elements = nfIO $ do
  oset <- fromList seed height $ concat elements
  mvars <- mapM (myForkIO . mapM_ (flip contains oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


destructiveDelete :: StdGen -> [[Int]] -> IO ()
destructiveDelete seed elements = nfIO $ do
  oset <- fromList seed height $ concat elements
  mvars <- mapM (myForkIO . mapM_ (flip delete oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


pureInsert :: [[Int]] -> IO ()
pureInsert elements = nfIO $ do
  root <- newMVar $ S.empty
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ S.insert e s))


pureContains :: [[Int]] -> IO ()
pureContains elements = nfIO $ do
  root <- newMVar $ S.fromList $ concat elements
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ if S.member e s then s else s))


pureDelete :: [[Int]] -> IO ()
pureDelete elements = nfIO $ do
  root <- newMVar $ S.fromList $ concat elements
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ S.delete e s))


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
      [ bench "insert" (destructiveInsert seed elems1)
      , bench "contains" (destructiveContains seed elems1)
      , bench "delete" (destructiveDelete seed elems1)
      ]
    , bgroup "destructive 2"
      [ bench "insert" (destructiveInsert seed elems2)
      , bench "contains" (destructiveContains seed elems2)
      , bench "delete" (destructiveDelete seed elems2)
      ]
    , bgroup "destructive 3"
      [ bench "insert" (destructiveInsert seed elems3)
      , bench "contains" (destructiveContains seed elems3)
      , bench "delete" (destructiveDelete seed elems3)
      ]
    , bgroup "destructive 4"
      [ bench "insert" (destructiveInsert seed elems4)
      , bench "contains" (destructiveContains seed elems4)
      , bench "delete" (destructiveDelete seed elems4)
      ]
    , bgroup "pure 1"
      [ bench "insert" (pureInsert elems2)
      , bench "contains" (pureContains elems2)
      , bench "delete" (pureDelete elems2)
      ]
    , bgroup "pure 2"
      [ bench "insert" (pureInsert elems2)
      , bench "contains" (pureContains elems2)
      , bench "delete" (pureDelete elems2)
      ]
    , bgroup "pure 3"
      [ bench "insert" (pureInsert elems3)
      , bench "contains" (pureContains elems3)
      , bench "delete" (pureDelete elems3)
      ]
    , bgroup "pure 4"
      [ bench "insert" (pureInsert elems4)
      , bench "contains" (pureContains elems4)
      , bench "delete" (pureDelete elems4)
      ]
    ]
