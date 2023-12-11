import System.Random

roll :: Int -> IO (Int)
roll upLimit = randomRIO (0,upLimit)

mroll :: Int -> Int -> IO ()
mroll t c = 
    if (c == 0) 
        then putStr "\nDone Rolling. \n"
    else
        (roll t) >>= \x -> 
            putStr ((show x)++", ") >>
            mroll t (c-1)

rollLoop :: IO ()
rollLoop =
    putStr "\nWhat kind of die should be rolled? (q for quit)" >>
    getLine >>= \input ->
        if (input /= "q") then
            (roll (read input :: Int)) >>= \out ->                
                putStr (show out) >>                
                rollLoop
        else
            putStr "\nDone.\n"

mrollLoop :: IO ()
mrollLoop =
    putStr "\nWhat kind of die should be rolled? (q for quit)\n" >>
    getLine >>= \t ->
        if (t /= "q") then
            putStr "\nHow many times should the die be rolled?\n" >>
            getLine >>= \c ->
                (mroll (read t :: Int) (read c :: Int)) >>= \out ->                
                putStr (show out) >>                
                mrollLoop            
        else
            putStr "\nDone.\n"