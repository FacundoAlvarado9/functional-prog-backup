import Control.Monad.State.Lazy
import Data.Map (Map, lookup, insert)
import qualified Data.Map as Map

data Faces = U | D | L | R
data Turns = CW | CounterCW
data Ant = Ant (Int, Int) Faces
data Field = Field (Map (Int, Int) Bool) Ant

instance Show Turns where
    show CW = "cw"
    show CounterCW = "ccw"

instance Show Faces where
    show U = "U"
    show D = "D"
    show L = "L"
    show R = "R"

-- Field sería el estado
-- String sería la acción "cw R-U"

--actualizar cuadro del mapa
--actualizar posicion hormiga según richtung
    -- cuidando no salirme de la grilla
--actualizar richtung
    -- si la nueva posición es negra (True) -> counter-clockwise
    -- sino --> clockwise

updtFacingDir :: Faces -> Turns -> Faces
updtFacingDir U CW = R
updtFacingDir U CounterCW = L

updtFacingDir R CW = D
updtFacingDir R CounterCW = U

updtFacingDir D CW = L
updtFacingDir D CounterCW = R

updtFacingDir L CW = U
updtFacingDir L CounterCW = D

{-
Actualizar posición
    Up -> (i-1, j) // Mind zero
    Down -> (i+1, j) // Mind max
    Right -> (i, j+1) // Mind max
    Left -> (i, j-1) // Mind zero
-}

newPos :: (Int, Int) -> Faces -> (Int, Int)
newPos (i,j) U = (i-1, j)
newPos (i,j) R = (i, j+1)
newPos (i,j) D = (i+1, j)
newPos (i,j) L = (i-1, j-1)

genOutput :: Turns -> Faces -> Faces -> String
genOutput turn oldFacingDir newFacingDir = show turn++" "++show oldFacingDir++"-"++show newFacingDir

step :: State Field String
step = state walk


walk :: Field -> (String, Field)
walk (Field g (Ant pos f))
    | (Data.Map.lookup pos g == Just False) = ((genOutput CW f (updtFacingDir f CW)), (Field (Data.Map.insert pos True g) (Ant (newPos pos f) (updtFacingDir f CW) )))
    | otherwise = ((genOutput CW f (updtFacingDir f CW)), (Field g (Ant (newPos pos f) (updtFacingDir f CounterCW))))

-- "moves one step in the current direction and changing the orientation"
-- tengo que ver donde estoy viendo nada mas
-- porque nada mas me tengo que mover, después cambio de dirección
--
-- updateAntPos :: Ant -> Turns -> Ant
-- updateAntPos (Ant (x,y) f) t
--     | 