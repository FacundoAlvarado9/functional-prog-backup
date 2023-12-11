import System.Directory
import Data.Char

alter :: IO ()
alter =
    putStr "Please, enter a file name:\n" >>
    getLine >>= \fname ->
        doesFileExist fname >>= \exists ->
        if (exists) then
            readFile fname >>= \contents ->
                return (reverse contents) >>= \reversed ->
                    return (map toUpper reversed) >>= \resul -> 
                        writeFile ("U."++fname) resul
        else
            putStr "File doesn't exist!\n"