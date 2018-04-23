module LifeGame where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Debug.Trace
import           System.Random

-- | Cell State
data Cell = Alive | Empty | Wall deriving (Eq)
-- | Field
newtype Field = Field {runField :: [[Cell]]}
-- | Position
type Pos = (Int, Int)
-- | Size of field
type Size = (Int, Int)

-- | Return cell at the specified position
at :: Field -> Pos -> Cell
Field field `at` (i, j) = {-trace (show (i+1) ++ " " ++ show (j+1)) $-} field !! (i+1) !! (j+1)

instance Show Cell where
  show Alive = "*"
  show Empty = " "
  show Wall  = "-"

instance Show Field where
  show (Field []) = ""
  show (Field(row:rows)) = (foldl (\s c -> s ++ show c) "" row) ++ "\n" ++ show (Field rows)

gameStart :: Field -> Size-> (Field -> IO ()) -> IO ()
gameStart field (w, h) action = do
  action field
  let next = wrapWall (w,h) $ [[ nextCell field (i, j) | j <- [0..w-1]]| i <- [0..h-1]]
   in gameStart next (w,h) action

-- | Count the number of alive cells around the position
aliveCntAround :: Field -> Pos -> Int
aliveCntAround field (i, j) = foldl (\s e -> if e == Alive then s+1 else s) 0 $ map (field `at`) [(i-1, j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1),(i+1,j-1),(i+1,j),(i+1,j+1)]

-- | Return next generate cell
nextCell :: Field -> Pos -> Cell
nextCell field pos
  | nowCell == Empty && aroundCnt == 3       = Alive
  | nowCell == Alive && elem aroundCnt [2,3] = Alive
  | nowCell == Alive && aroundCnt <=1        = Empty
  | nowCell == Alive && aroundCnt >= 4       = Empty
  | otherwise                                = Empty
  where
    nowCell   = field `at` pos
    aroundCnt = aliveCntAround field pos


-- | Empty field
emptyField :: Size -> Field
emptyField (w, h) = wrapWall (w,h) (replicate h $ replicate w Empty)

-- | Generate random field
randomField :: Size -> IO Field
randomField (w, h) = do
  rows <- forM [0..h] $ \_ -> do
    g <- newStdGen
    let bools = take w (randoms g) :: [Bool]
        row = map (\e -> if e then Alive else Empty) bools
    return row
  return $ wrapWall (w,h) rows

-- | Create field by wrapping with walls
wrapWall :: Size -> [[Cell]]  -> Field
wrapWall (w,h) rows =
  Field $ wallRow : (map (\row -> Wall : row ++ [Wall]) rows) ++ [wallRow]
  where wallRow = replicate (w+2) Wall
