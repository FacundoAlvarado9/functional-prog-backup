import Data.Attoparsec.Char8

data Implication = Implication [String] [String] deriving Show

sourceFile :: FilePath
sourceFile = "impl.txt"

type Result = [Implication]
