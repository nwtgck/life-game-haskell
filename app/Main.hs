module Main where

import Control.Concurrent
import System.Environment
import System.Process

import LifeGame

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
  else putStrLn "Specify size of the field"

  return ()
