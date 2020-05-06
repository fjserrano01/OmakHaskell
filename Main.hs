--Fernando Serrano, Omok game, PL
module Main where
 import System.IO
 import System.Random
 import BoardElements
 import StoneandMovement
 import UiOmokGame

 bd = mkBoard 16
 player = mkPlayer
 --To run game type playingGame bd player in terminal
 main = do
  playingGame bd player

  --putStrLn "Enter x y (1-15 or -1 to quit)?"