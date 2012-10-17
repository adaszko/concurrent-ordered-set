import qualified Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Concurrent.OrderedMap


type ElementType = Int


prop_sorts :: Property
prop_sorts = monadicIO $ do
  contents <- pick (arbitrary :: Gen [ElementType])
  list <- run (fromList 1 contents >>= toList)
  assert $ list == Data.List.nub contents

{-
prop_inserts :: Property
prop_inserts = monadicIO $ do
  contents <- pick (arbitrary :: Gen [ElementType])
  element <- pick (arbitrary :: Gen ElementType)
  let sorted = sort contents
  list <- run $ do
    orderedMap <- fromList 1 contents
    inserted <- insert element orderedMap
    toList orderedMap
    return (inserted, 
  assert $ list == Data.List.insert 
-}

-- prop_deletes
-- prop_finds


main = do
  quickCheck prop_sorts
