module DequeTests (deque) where

import Deque
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

-- peekFront from empty Deque is undefined
prop1 :: TestTree
prop1 = QC.testProperty "read_empty" $
    peekFront ([] :: [Int]) == Nothing

-- There is a LIFO relation between PeekFront and PushFront
prop2_1 :: TestTree
prop2_1 = QC.testProperty "lifo_front_1" $ \a xs ->
    peekFront (pushFront a (xs :: Deque Int)) == Just a

prop2_2 :: TestTree
prop2_2 = QC.testProperty "lifo_front_2" $ \a xs ->
    popFront (pushFront a (xs :: Deque Int)) == xs

-- There is a FIFO relation between PeekFront and PushEnd
prop3_1 :: TestTree
prop3_1 = QC.testProperty "fifo_front_end_1" $ \a ->
    peekFront (pushEnd a ([] :: [Int])) == Just a

prop3_2 :: TestTree
prop3_2 = QC.testProperty "fifo_front_end_2" $ \a ->
    popFront (pushEnd a ([] :: [Int])) == []

prop3_3 :: TestTree
prop3_3 = QC.testProperty "fifo_front_end_3" $ \a xs->
    not(null xs) ==> peekFront (pushEnd a (xs :: [Int])) == (peekFront xs)

prop3_4 :: TestTree
prop3_4 = QC.testProperty "fifo_front_end_4" $ \a xs->
    not(null xs) ==> popFront (pushEnd a (xs :: [Int])) == pushEnd a (popFront xs)

-- pushing anything on any end of an empty Deque results in a non-empty Deque
prop4_1 :: TestTree
prop4_1 = QC.testProperty "pushFront_empty_results_nonEmpty" $ \a ->
    pushFront a ([] :: Deque Int) /= ([] :: Deque Int)

prop4_2 :: TestTree
prop4_2 = QC.testProperty "pushEnd_empty_results_nonEmpty" $ \a ->
    pushEnd a ([] :: Deque Int) /= ([] :: Deque Int)

-- Pushing sth at the end of an non-empty list does not affect the result of PeekFront
prop5 :: TestTree
prop5 = QC.testProperty "pushEnd_not_affect_peekFront" $ \a xs->
    not(null xs) ==> peekFront (pushEnd a (xs :: [Int])) == peekFront (xs :: [Int])

-- Pushing sth at the start of an non-empty list does not affect the result of PeekEnd
prop6 :: TestTree
prop6 = QC.testProperty "pushFront_not_affect_peekEnd" $ \a xs->
    not(null xs) ==> peekEnd (pushFront a (xs :: [Int])) == peekEnd (xs :: [Int])

-- PushFront alters the previous value of PeekFront
-- PushEnd alters the previous value of PeekEnd

-- There is a LIFO relation between PeekEnd and PushEnd
-- There is a FIFO relation between PeekEnd and PushFront

-- pushing at the back and peeking at the front on NON-EMPTY is the same as
--      peeking at the front not having pushed at the back

deque = testGroup "All deque tests"
    [ prop1,prop2_1, prop2_2, prop3_1, prop3_2, prop3_3, prop3_4, prop4_1, prop4_2, prop5, prop6 ]

run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $ deque