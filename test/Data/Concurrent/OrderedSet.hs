import qualified Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Concurrent.OrderedSet
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception
import Control.Monad


type ElementType = Int


genElem :: Gen ElementType
genElem = choose (-100, 100)


genLevel :: Gen Int
genLevel = choose (1, 10)


genThreads :: PropertyM IO Int
genThreads = do
  numCaps <- run getNumCapabilities
  pick $ choose (1, 2*numCaps)


uniq :: Ord a => [a] -> [a]
uniq = map head . Data.List.group . Data.List.sort


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  forkIO (io `Control.Exception.finally` putMVar mvar ())
  return mvar


prop_empty :: Property
prop_empty = monadicIO $ do
  level <- pick genLevel
  result <- run $ do
    oset <- empty level
    toList oset
  assert $ null result


prop_trivial_one_level_insert_0_1 :: Property
prop_trivial_one_level_insert_0_1 = monadicIO $ do
  let level = 1
  result <- run $ do
    oset <- empty level
    insert 0 oset
    insert 1 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_one_level_insert_1_0 :: Property
prop_trivial_one_level_insert_1_0 = monadicIO $ do
  let level = 1
  result <- run $ do
    oset <- empty level
    insert 1 oset
    insert 0 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_two_level_insert_0_1 :: Property
prop_trivial_two_level_insert_0_1 = monadicIO $ do
  result <- run $ do
    oset <- empty 2
    insert 0 oset
    insert 1 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_two_level_insert_1_0 :: Property
prop_trivial_two_level_insert_1_0 = monadicIO $ do
  result <- run $ do
    oset <- empty 2
    insert 1 oset
    insert 0 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_one_level_insert_delete_0 :: Property
prop_trivial_one_level_insert_delete_0 = monadicIO $ do
  result <- run $ do
    oset <- empty 0
    insert 0 oset
    delete 0 oset
    toList oset
  assert $ null result


prop_sortsElimsDups :: Property
prop_sortsElimsDups = monadicIO $ do
  contents <- pick $ listOf genElem
  level <- pick genLevel
  result <- run $ do
    oset <- fromList level contents
    toList oset
  assert $ result == uniq contents


prop_inserts :: Property
prop_inserts = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  (result, inserted) <- run $ do
    oset <- fromList level contents
    inserted <- insert element oset
    result <- toList oset
    return (result, inserted)
  assert $ result == uniq (element : contents)
  assert $ inserted /= elem element contents


prop_inserts_very_concurrently :: Property
prop_inserts_very_concurrently = monadicIO $ do
  contents <- pick $ listOf genElem
  numThreads <- genThreads
  level <- pick genLevel
  elements <- replicateM numThreads $ pick $ listOf genElem
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elem -> myForkIO $ mapM_ (flip insert oset) elem) elements
    mapM_ takeMVar mvars
    toList oset
  let expected = uniq $ contents ++ concat elements
  assert $ result == expected


prop_inserts_concurrently :: Property
prop_inserts_concurrently = monadicIO $ do
  let contents = []
  let level = 1
  let elements = [[1], [2]]
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elem -> myForkIO $ mapM_ (flip insert oset) elem) elements
    mapM_ takeMVar mvars
    toList oset
  let expected = uniq $ contents ++ concat elements
  assert $ result == expected



prop_contains :: Property
prop_contains = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  found <- run $ do
    oset <- fromList level contents
    contains element oset
  assert $ found == elem element contents


prop_deletes :: Property
prop_deletes = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  (result, deleted) <- run $ do
    oset <- fromList level contents
    deleted <- delete element oset
    result <- toList oset
    return (result, deleted)
  assert $ result == (Data.List.delete element $ uniq contents)
  assert $ deleted == elem element contents


prop_deletes_very_concurrently :: Property
prop_deletes_very_concurrently = monadicIO $ do
  contents <- pick $ listOf genElem
  numThreads <- genThreads
  level <- pick genLevel
  elements <- replicateM numThreads $ pick $ listOf genElem
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elem -> myForkIO $ mapM_ (flip delete oset) elem) elements
    mapM_ takeMVar mvars
    toList oset
  let expected = foldr Data.List.delete (uniq contents) $ concat elements
  assert $ result == expected


prop_deletes_concurrently :: Property
prop_deletes_concurrently = monadicIO $ do
  let contents = [1,2]
  let level = 1
  let elements = [[1], [2]]
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elem -> myForkIO $ mapM_ (flip delete oset) elem) elements
    mapM_ takeMVar mvars
    toList oset
  let expected = foldr Data.List.delete (uniq contents) $ concat elements
  assert $ result == expected


main = do
  quickCheck prop_empty

  -- TODO: indicate somehow that these test cases are deterministic
  quickCheck prop_trivial_one_level_insert_0_1
  quickCheck prop_trivial_one_level_insert_1_0
  quickCheck prop_trivial_two_level_insert_0_1
  quickCheck prop_trivial_two_level_insert_1_0

  quickCheck prop_sortsElimsDups
  quickCheck prop_inserts
  quickCheck prop_contains
  quickCheck prop_deletes

  quickCheck prop_inserts_concurrently
  quickCheck prop_deletes_concurrently
  quickCheck prop_inserts_very_concurrently
  quickCheck prop_deletes_very_concurrently
