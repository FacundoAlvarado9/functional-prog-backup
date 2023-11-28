import TicTacToe
import Data.Map (fromList)

test = wonARow ((Grid (fromList([(0,"a"), (1,"a"), (2,"a"), (3,""), (4,""),(5,""), (6,""), (7,""), (8,"")])))) "a"

firstMove = doTurn (newGame "a" "b") 0
sameMove = doTurn (firstMove) 0 --Place already has a token
moveOutsideGrid = doTurn (newGame "a" "b") 16 --Invalid grid position

secondMove = doTurn (firstMove) 4
thirdMove = doTurn (secondMove) 1
fourthMove = doTurn (thirdMove) 7
winningMove = doTurn (fourthMove) 2
moveAfterWon = doTurn (winningMove) 5

testDoTurns = doTurns (newGame "a" "b") [0,4,1,7,2,5]