module SLTests where

import SortedList
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (SortedList a) where
    arbitrary = do
        size <- QC.choose (0, 20)
        els  <- QC.vector size
        return $ foldr insert' empty' els

slProp1 :: TestTree
slProp1 = QC.testProperty "minimum_is_head" $ \(xs :: SortedList Int) ->
    not(null' xs) ==> minimum' (xs) == head'(xs)

slProp2 :: TestTree
slProp2 = QC.testProperty "maximum_is_last" $ \(xs :: SortedList Int) ->
    not(null' xs) ==> maximum' (xs) == last'(xs)

slProp3 :: TestTree
slProp3 = QC.testProperty "adding_elem_add_1_length" $ \(x :: Int) (xs :: SortedList Int) ->
    length' (insert' x xs) == (length' xs) + 1

-- slProp4 :: TestTree
-- slProp4 = QC.testProperty "concat_of_two_ordered_with_conditions" $ \(xs :: SortedList Int) (ys :: SortedList Int)->
--     (last' xs) < (head' ys) ==> maximum' (xs) == last'(xs)

sortedList = testGroup "All sortedList tests"
    [ slProp1, slProp2, slProp3 ]

run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $ sortedList