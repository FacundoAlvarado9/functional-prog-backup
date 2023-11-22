--- (1)
data Jeton = Red | Green | Blue | Silver | Gold

value :: Jeton -> Int
value a = case a of
    Red -> 1
    Green -> 5
    Blue -> 10
    Silver -> 50
    Gold -> 100

-- (2)
data Jetons = EmptyJetons | Jetons Jeton Jetons

count :: Jetons -> Int
count EmptyJetons = 0
count (Jetons jeton jc) = 1 + count jc

payoff :: Jetons -> Int
payoff EmptyJetons = 0
payoff (Jetons jeton jc) = value (jeton) + payoff jc

-- (3)

maxJetonUnder :: Int -> Jeton
maxJetonUnder w
    | w >= 100 = Gold
    | w < 100 && w >= 50 = Silver
    | w < 50 && w >= 10 = Blue
    | w < 10 && w >= 5 = Green
    | w < 5 && w >= 1 = Red
    --use otherwise

buy :: Int -> Jetons
buy 0 = EmptyJetons

-- A collection consisting of:
--- The biggest Jeton under the given value
--- The buy of the remaining value
buy w = (Jetons (maxJetonUnder(w)) (buy(w-value(maxJetonUnder(w)))) )

--show function
instance Show Jeton where
    show j = case j of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        Silver -> "silver"
        Gold -> "gold"