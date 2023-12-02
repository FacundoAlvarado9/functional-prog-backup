module StackTest where

import Stack
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

-- Instance of Arbitrary for Stack. 
-- Generates a stack of random length between 10 and 20,
-- filled with random values
instance QC.Arbitrary a=> QC.Arbitrary (St a) where
  arbitrary = do
    size <- QC.choose (0, 20) 
    els  <- QC.vector size     
    return $ foldr push empty els

run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $
    testGroup "All stack tests"
    [ QC.testProperty "top_push" $ \a s ->
        top (push a (s :: St Int)) == a
    , QC.testProperty "pop_push" $ \a s ->
        pop (push a (s :: St Int)) == s
    , QC.testProperty "push not empty" $ \a s ->
        empty /= push a (s:: St Int)
    ]

