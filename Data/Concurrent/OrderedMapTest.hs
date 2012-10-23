import qualified Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Concurrent.OrderedMap


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
  --quickCheck prop_empty
  verboseCheck prop_inserts
  --verboseCheck prop_sortsElimsDups
  --quickCheck prop_contains
  --quickCheck prop_deletes
