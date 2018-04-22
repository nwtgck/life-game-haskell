module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Debug.Trace
import           System.Console.ANSI
import           System.Environment
import           System.Process
import           System.Random

-- マスの状態の値
data Cell = Alive | Empty | Wall deriving (Eq)
-- フィールド全体
newtype Field = Field {runField :: [[Cell]]}
-- 位置
type Pos = (Int, Int)
-- フィールドのサイズ
type Size = (Int, Int)

-- フィールドの位置の場所のCellを返す
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

-- posの場所にある生きているセルを数える
aliveCntAround :: Field -> Pos -> Int
aliveCntAround field (i, j) = foldl (\s e -> if e == Alive then s+1 else s) 0 $ map (field `at`) [(i-1, j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1),(i+1,j-1),(i+1,j),(i+1,j+1)]

-- 今の場所から次のセルを返す
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


-- 空のフィールド
emptyField :: Size -> Field
emptyField (w, h) = wrapWall (w,h) (replicate h $ replicate w Empty)

-- ランダムなフィールドを返す
randomField :: Size -> IO Field
randomField (w, h) = do
  rows <- forM [0..h] $ \_ -> do
    g <- newStdGen
    let bools = take w (randoms g) :: [Bool]
        row = map (\e -> if e then Alive else Empty) bools
    return row
  return $ wrapWall (w,h) rows

-- 壁で囲むんでFieldを作る
wrapWall :: Size -> [[Cell]]  -> Field
wrapWall (w,h) rows =
  Field $ wallRow : (map (\row -> Wall : row ++ [Wall]) rows) ++ [wallRow]
  where wallRow = replicate (w+2) Wall

main :: IO ()
main = do
  (args) <- getArgs
  if (length args == 2) then do
    let (wS:hS:_) = args
    let (w, h) = (read wS :: Int, read hS :: Int)
    let size = (w, h)
    field <- randomField size
    gameStart field size $ \field -> do
      system "clear"
      print field
      threadDelay 30000
  else putStrLn "大きさを指定してください"

  return ()
