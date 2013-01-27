module Main where

import qualified Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Concurrent.OrderedSet
import Control.Concurrent
import qualified Control.Exception
import Control.Monad
import Test.Framework
import Test.Framework.Providers.QuickCheck2


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
  _ <- forkIO (io `Control.Exception.finally` putMVar mvar ())
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
    oset <- empty level :: IO (OrderedSet Int)
    _ <- insert 0 oset
    _ <- insert 1 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_one_level_insert_1_0 :: Property
prop_trivial_one_level_insert_1_0 = monadicIO $ do
  let level = 1
  result <- run $ do
    oset <- empty level :: IO (OrderedSet Int)
    _ <- insert 1 oset
    _ <- insert 0 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_two_level_insert_0_1 :: Property
prop_trivial_two_level_insert_0_1 = monadicIO $ do
  result <- run $ do
    oset <- empty 2 :: IO (OrderedSet Int)
    _ <- insert 0 oset
    _ <- insert 1 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_two_level_insert_1_0 :: Property
prop_trivial_two_level_insert_1_0 = monadicIO $ do
  result <- run $ do
    oset <- empty 2 :: IO (OrderedSet Int)
    _ <- insert 1 oset
    _ <- insert 0 oset
    toList oset
  assert $ result == [0, 1]


prop_trivial_one_level_insert_delete_0 :: Property
prop_trivial_one_level_insert_delete_0 = monadicIO $ do
  result <- run $ do
    oset <- empty 0 :: IO (OrderedSet Int)
    _ <- insert 0 oset
    _ <- delete 0 oset
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
  elems <- replicateM numThreads $ pick $ listOf genElem
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elt -> myForkIO $ mapM_ (flip insert oset) elt) elems
    mapM_ takeMVar mvars
    toList oset
  let expected = uniq $ contents ++ concat elems
  assert $ result == expected


prop_inserts_concurrently :: Property
prop_inserts_concurrently = monadicIO $ do
  let contents = [] :: [Int]
  let level = 1
  let elems = [[1], [2]]
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elt -> myForkIO $ mapM_ (flip insert oset) elt) elems
    mapM_ takeMVar mvars
    toList oset
  let expected = uniq $ contents ++ concat elems
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
  elems <- replicateM numThreads $ pick $ listOf genElem
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elt -> myForkIO $ mapM_ (flip delete oset) elt) elems
    mapM_ takeMVar mvars
    toList oset
  let expected = foldr Data.List.delete (uniq contents) $ concat elems
  assert $ result == expected


prop_deletes_concurrently :: Property
prop_deletes_concurrently = monadicIO $ do
  let contents = [1,2] :: [Int]
  let level = 1
  let elems = [[1], [2]]
  result <- run $ do
    oset <- fromList level contents
    mvars <- mapM (\elt -> myForkIO $ mapM_ (flip delete oset) elt) elems
    mapM_ takeMVar mvars
    toList oset
  let expected = foldr Data.List.delete (uniq contents) $ concat elems
  assert $ result == expected


properties :: [Test]
properties = [
  testProperty "empty" prop_empty,

  -- TODO: indicate somehow that these are deterministic
  testProperty "1_insert_0_1" prop_trivial_one_level_insert_0_1,
  testProperty "1_insert_1_0" prop_trivial_one_level_insert_1_0,
  testProperty "2_insert_0_1" prop_trivial_two_level_insert_0_1,
  testProperty "2_insert_1_0" prop_trivial_two_level_insert_1_0,

  testProperty "sortsElimsDups" prop_sortsElimsDups,
  testProperty "inserts" prop_inserts,
  testProperty "contains" prop_contains,
  testProperty "deletes" prop_deletes,

  testProperty "inserts_concurrently" prop_inserts_concurrently,
  testProperty "inserts_very_concurrently" prop_inserts_very_concurrently,

  testProperty "deletes_concurrently" prop_deletes_concurrently,
  testProperty "deletes_very_concurrently" prop_deletes_very_concurrently
  ]


main :: IO ()
main = defaultMain properties
