import qualified Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Concurrent.OrderedSet


-- TODO: Test cases involving forkIO


type ElementType = Int


genElem :: Gen ElementType
genElem = choose (-100, 100)


genLevel :: Gen Int
genLevel = choose (1, 10)


uniq :: Ord a => [a] -> [a]
uniq = map head . Data.List.group . Data.List.sort


prop_empty :: Property
prop_empty = monadicIO $ do
  level <- pick genLevel
  result <- run $ do
    omap <- empty level
    toList omap
  assert $ null result


prop_trivial_one_level_insert_0_1 :: Property
prop_trivial_one_level_insert_0_1 = monadicIO $ do
  let level = 1
  result <- run $ do
    omap <- empty level
    insert 0 omap
    insert 1 omap
    toList omap
  assert $ result == [0, 1]


prop_trivial_one_level_insert_1_0 :: Property
prop_trivial_one_level_insert_1_0 = monadicIO $ do
  let level = 1
  result <- run $ do
    omap <- empty level
    insert 1 omap
    insert 0 omap
    toList omap
  assert $ result == [0, 1]


prop_trivial_two_level_insert_0_1 :: Property
prop_trivial_two_level_insert_0_1 = monadicIO $ do
  result <- run $ do
    omap <- empty 2
    insert 0 omap
    insert 1 omap
    toList omap
  assert $ result == [0, 1]


prop_trivial_two_level_insert_1_0 :: Property
prop_trivial_two_level_insert_1_0 = monadicIO $ do
  result <- run $ do
    omap <- empty 2
    insert 1 omap
    insert 0 omap
    toList omap
  assert $ result == [0, 1]


prop_trivial_one_level_insert_delete_0 :: Property
prop_trivial_one_level_insert_delete_0 = monadicIO $ do
  result <- run $ do
    omap <- empty 0
    insert 0 omap
    delete 0 omap
    toList omap
  assert $ null result


prop_sortsElimsDups :: Property
prop_sortsElimsDups = monadicIO $ do
  contents <- pick $ listOf genElem
  level <- pick genLevel
  result <- run $ do
    omap <- fromList level contents
    toList omap
  assert $ result == uniq contents


prop_inserts :: Property
prop_inserts = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  (result, inserted) <- run $ do
    omap <- fromList level contents
    inserted <- insert element omap
    result <- toList omap
    return (result, inserted)
  assert $ result == uniq (element : contents)
  assert $ inserted /= elem element contents


prop_contains :: Property
prop_contains = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  found <- run $ do
    omap <- fromList level contents
    contains element omap
  assert $ found == elem element contents


prop_deletes :: Property
prop_deletes = monadicIO $ do
  contents <- pick $ listOf genElem
  element <- pick genElem
  level <- pick genLevel
  (result, deleted) <- run $ do
    omap <- fromList level contents
    deleted <- delete element omap
    result <- toList omap
    return (result, deleted)
  assert $ result == (Data.List.delete element $ uniq contents)
  assert $ deleted == elem element contents


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
