import System.Random

generateList :: IO [Int]
generateList = sequence (replicate 24 (randomRIO (0,99 :: Int))) -- Random puede insertar duplicados

cutLine :: Int -> Bool
cutLine n
    | (n==4) || (n==0) || (n==14) || (n==19) = True
    | otherwise = False

-- se puede formatear usando take 5 y fold para intercalar los "|" para cada linea
-- mientras hacemos drop de los que ya imprimimos
-- útil usar unlines -> utiliza una lista de strings e intercala \n
-- usar append para que se agregue al final de la lista, no se renueve el archivo.
-- agregar los - entre las líneas

test :: Int -> [Int] -> Strin -> IO (String)
test i l s = do
    if (i<24)
        test i-1 (tail l) 

main :: IO ()
main = test 24