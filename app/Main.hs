module Main where

import Control.Concurrent
import System.Environment
import System.Process
import qualified UI.HSCurses.Curses as Curses

import LifeGame

main :: IO ()
main = do
  (args) <- getArgs
  if (length args == 2) then do
    let (wS:hS:_) = args
    let (w, h) = (read wS :: Int, read hS :: Int)
    let size = (w, h)
    field <- randomField size
    Curses.initCurses
    gameStart field size $ \field -> do
      Curses.erase
      let !fieldStr = show field
      Curses.wAddStr Curses.stdScr fieldStr
      Curses.refresh
      threadDelay 30000
    Curses.endWin
  else putStrLn "Specify size of the field"

  return ()
