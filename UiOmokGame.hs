module UiOmokGame where 
 import System.IO
 import System.Random
 import BoardElements
 import StoneandMovement
  --For reading user input
 playerToChar p =
  case p of
  1 -> 'O'
  2 -> 'X'
  x -> '.'
 
 --readInts :: IO [Int]
 --readInts = fmap (map read.words) getLine

 
 readXY bd p = do
  putStrLn "Enter Position: "
  line <- fmap (map read.words) getLine
  let parsed = line in do
    if length parsed == 2 then do
      let x = head parsed
          new = tail parsed
          y = head new in 
           if isEmpty x y bd then return (x, y)
            else readXY bd p 
     else readXY bd p

 --getX parsed = do 
  --let (x,_) = parsed in return x
 --getY parsed = do
  --let (_, y) = parsed in return y
 bd = mkBoard 15
 playingGame = do 
  --p <- mkPlayer
  positionPlayer <- readXY bd 'O'
  let x = fst positionPlayer
      y = snd positionPlayer 
  --bd <- mark x y bd 'O'
  putStrLn (boardToStr playerToChar bd)
  playingGame
