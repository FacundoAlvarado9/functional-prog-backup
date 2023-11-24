module TicTacToe where

import Data.Map (Map, empty, insert, member, fromList,lookup,isSubmapOf, elems)
import Data.List
import MSplitL

data Game = Game Grid String String String Int

data Grid = Grid (Map Int String)

instance Show Game where
    show (Game grid s t1 t2 t) = s ++ "\n" ++ show grid ++ "\n" ++ "Turn: " ++ show t


instance Show Grid where
    show (Grid m) = (intercalate "\n" (Data.List.map show x))
        where x = (mSplitL [2,5] (elems m))


newGame :: String -> String -> Game
newGame t1 t2 = Game (Grid (fromList([(0," "), (1," "), (2," "), (3," "), (4," "),(5," "), (6," "), (7," "), (8," ")]))) "El juego no terminó" t1 t2 0

wonARow :: Grid -> String -> Bool
wonARow (Grid m) tok
    | (isSubmapOf (fromList([(0,tok), (1,tok), (2,tok)])) m) = True -- 1st Row
    | (isSubmapOf (fromList([(3,tok), (4,tok), (5,tok)])) m) = True -- 2nd Row
    | (isSubmapOf (fromList([(6,tok), (7,tok), (8,tok)])) m) = True -- 3rd Row
    | otherwise = False

wonACol :: Grid -> String -> Bool
wonACol (Grid m) tok
    | (isSubmapOf (fromList([(0,tok), (3,tok), (6,tok)])) m) = True -- 1st Col
    | (isSubmapOf (fromList([(1,tok), (4,tok), (7,tok)])) m) = True -- 2nd Col
    | (isSubmapOf (fromList([(2,tok), (5,tok), (8,tok)])) m) = True -- 3rd Col
    | otherwise = False

wonADiagonal :: Grid -> String -> Bool
wonADiagonal (Grid m) tok
    | (isSubmapOf (fromList([(0,tok), (4,tok), (8,tok)])) m) = True -- 1st Diag
    | (isSubmapOf (fromList([(6,tok), (4,tok), (2,tok)])) m) = True -- 2nd Diag
    | otherwise = False

test = wonARow ((Grid (fromList([(0,"a"), (1,"a"), (2,"a"), (3,""), (4,""),(5,""), (6,""), (7,""), (8,"")])))) "a"

checkWinner :: Grid -> String -> String -> String
checkWinner (Grid m) t1 t2
    | (wonARow (Grid m) t1) || (wonACol (Grid m) t1) || (wonADiagonal (Grid m) t1) = ("Player " ++ t1 ++ " won")
    | (wonARow (Grid m) t2) || (wonACol (Grid m) t2) || (wonADiagonal (Grid m) t2) = ("Player " ++ t2 ++ " won")
    | otherwise = "Game hasn't finished yet"

doTurn :: Game -> Int -> Game
doTurn (Game (Grid m) s t1 t2 t) k
    | (s == "Player " ++ t1 ++ " won") || (s == "Player " ++ t2 ++ " won") = (Game (Grid m) s t1 t2 t)
    | (t > 10) = (Game (Grid m) "Draw" t1 t2 t)
    | (k<0) || (k>8) = (Game (Grid m) "Invalid grid position" t1 t2 t)
    | (Data.Map.lookup k m /= (Just " ")) = (Game (Grid m) "Place already has a token" t1 t2 t)
    | otherwise = if (even t)
        then executeMove (Game (Grid m) s t1 t2 t) t1 k
        else executeMove (Game (Grid m) s t1 t2 t) t2 k

executeMove :: Game -> String -> Int -> Game -- In order to checkWinner after the move
executeMove (Game (Grid m) s t1 t2 t) executer k = (Game (Grid m') (checkWinner (Grid m') t1 t2) t1 t2 (t+1))
    where m' = (Data.Map.insert k executer m)

-- Auxiliary definition for myself to understand the problem
doTurns' :: Game -> [Int] -> Game
doTurns' game [] = game
doTurns' game (k:ks) = doTurns (doTurn game k) ks

doTurns :: Game -> [Int] -> Game
doTurns game ks = foldl doTurn game ks

firstMove = doTurn (newGame "a" "b") 0
sameMove = doTurn (firstMove) 0 --Place already has a token
moveOutsideGrid = doTurn (newGame "a" "b") 16 --Invalid grid position

secondMove = doTurn (firstMove) 4
thirdMove = doTurn (secondMove) 1
fourthMove = doTurn (thirdMove) 7
winningMove = doTurn (fourthMove) 2
moveAfterWon = doTurn (winningMove) 5