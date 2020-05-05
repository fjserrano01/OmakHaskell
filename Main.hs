module Main where
 import System.IO
 import System.Random
 import BoardElements
 import StoneandMovement
 import UiOmokGame

 bd = mkBoard 15
 player = mkPlayer
 main = do
  playingGame bd player

  --putStrLn "Enter x y (1-15 or -1 to quit)?"