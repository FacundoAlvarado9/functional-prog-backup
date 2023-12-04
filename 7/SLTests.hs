module SLTests where

import SortedList
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

slProp1 :: TestTree
slProp1 = QC.testProperty "minimum_is_head" $ \xs ->
    not(null xs) ==> minimum(xs :: SortedList Int) == head'(xs)

-- prop2 :: TestTree
-- prop2 = QC.testProperty "alwaysSorted" $ \x xs ->
--     isSorted((x:xs) :: SortedList Int) == True

slProp5 :: TestTree
slProp5 = QC.testProperty "if_elem_smaller_than_head_then_list_is_ordered" $ \x xs ->
    (not(null xs) && ((x :: Int) <= head'(xs :: SortedList Int))) ==> (isSorted(x:xs))

sortedList = testGroup "All sortedList tests"
    [ slProp1, slProp5 ]

run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $ sortedList