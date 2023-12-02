{-# LANGUAGE FlexibleInstances #-}
module MapThinTest where

-- import MapFun
-- import MapList
import MapWeighted
import Prelude hiding (lookup)    

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

-- Instance of Arbitrary for Map. 
-- Generates a map of random length between 10 and 20,
-- filled with random values
instance (Ord a, QC.Arbitrary a, QC.Arbitrary b)=>
          QC.Arbitrary (Map a b) where
  arbitrary = do
    size <- QC.choose (0, 20)
    a <- QC.vector size
    b <- QC.vector size
    return (foldl (\s (a, v)-> put a (Just v) s) empty (zip a b))

-- Properties (thin interface)

-- Reading from the empty map
prop1 :: TestTree
prop1 = QC.testProperty "read_empty" $ \a-> 
  lookup a (empty :: Map Int String) == Nothing 
 
-- Reading at the same position
prop2 :: TestTree
prop2 = QC.testProperty "lookup_put eq" $ \a v s->
  lookup a (put a v (s :: Map Int String)) == v

-- Reading at a different position
prop3 :: TestTree
prop3 = QC.testProperty "lookup_put other" $ \a b v s->
  a /= b ==> lookup a (put b v s) ==
             lookup a (s :: Map Int String)

-- Writing at the same position
prop4 :: TestTree
prop4 = QC.testProperty "put_put eq" $ \a v w s->
  put a w (put a v s) == put a w (s :: Map Int String)

-- Writing at a different position
prop5 :: TestTree
prop5 = QC.testProperty "put_put other" $ \a v b w s->
  a /= b ==> put a v (put b w s) == 
             put b w (put a v s :: Map Int String)


run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $
    testGroup "All map tests"
    [prop1, prop2, prop3, prop4, prop5]

