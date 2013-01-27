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


destructiveInsert :: Int -> IO ()
destructiveInsert numThreads = nfIO $ do
  oset <- empty height
  elements <- genElems numThreads
  mvars <- mapM (myForkIO . mapM_ (flip insert oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


destructiveContains :: Int -> IO ()
destructiveContains numThreads = nfIO $ do
  elements <- genElems numThreads
  oset <- fromList height $ concat elements
  mvars <- mapM (myForkIO . mapM_ (flip contains oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


destructiveDelete :: Int -> IO ()
destructiveDelete numThreads = nfIO $ do
  elements <- genElems numThreads
  oset <- fromList height $ concat elements
  mvars <- mapM (myForkIO . mapM_ (flip delete oset)) elements
  mapM_ takeMVar mvars
  toList oset >>= return


pureInsert :: Int -> IO ()
pureInsert numThreads = nfIO $ do
  root <- newMVar $ S.empty
  elements <- genElems numThreads
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ S.insert e s))


pureContains :: Int -> IO ()
pureContains numThreads = nfIO $ do
  elements <- genElems numThreads
  root <- newMVar $ S.fromList $ concat elements
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ if S.member e s then s else s))


pureDelete :: Int -> IO ()
pureDelete numThreads = nfIO $ do
  elements <- genElems numThreads
  root <- newMVar $ S.fromList $ concat elements
  mvars <- mapM (myForkIO . thread root) elements
  mapM_ takeMVar mvars
    where
      thread root = mapM_ (\e -> modifyMVar_ root (\s -> return $ S.delete e s))


main :: IO ()
main = defaultMain
    [ bgroup "destructive 1"
      [ bench "insert" (destructiveInsert 1)
      , bench "contains" (destructiveContains 1)
      , bench "delete" (destructiveDelete 1)
      ]
    , bgroup "destructive 2"
      [ bench "insert" (destructiveInsert 2)
      , bench "contains" (destructiveContains 2)
      , bench "delete" (destructiveDelete 2)
      ]
    , bgroup "pure 1"
      [ bench "insert" (pureInsert 1)
      , bench "contains" (pureContains 1)
      , bench "delete" (pureDelete 1)
      ]
    , bgroup "pure 2"
      [ bench "insert" (pureInsert 2)
      , bench "contains" (pureContains 2)
      , bench "delete" (pureDelete 2)
      ]
    ]
