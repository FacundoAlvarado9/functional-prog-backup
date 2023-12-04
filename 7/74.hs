import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Deque
import SortedList

import DequeTests
import SLTests

main = defaultMain tests

tests = testGroup "Tests" [deque, sortedList]