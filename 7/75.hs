import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck((==>))

axiom1 :: TestTree
axiom1 = QC.testProperty "axiom_1" $ \ (as :: [Int]) ->
    (reverse . reverse) as == as

axiom2 :: TestTree
axiom2 = QC.testProperty "axiom_2" $ \ (as :: [Int]) (bs :: [Int]) ->
    (++) (reverse bs) (reverse as) == reverse ((++) as bs)

axiom3 :: TestTree
axiom3 = QC.testProperty "axiom_3" $ \ (as :: [Int]) ->
    not(null as) ==> (as == (head as):(tail as))

axiom4 :: TestTree
axiom4 = QC.testProperty "axiom_4" $ \ (as :: [Int]) (bs :: [Int]) ->
    (length as) + (length bs) == length ((++) as bs)

axiom5 :: TestTree
axiom5 = QC.testProperty "axiom_5" $ \ (as :: [Int]) ->
    not(null as) ==> ((++) ((reverse . tail) as) ([head as])) == (reverse as)

axiomPuzzleTests = testGroup "All axiomPuzzle tests"
    [ axiom1, axiom2, axiom3, axiom4, axiom5 ]

run =
  defaultMain $ localOption (QC.QuickCheckTests 5000) $ axiomPuzzleTests