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


 board = mkBoard 15

 playingGame bd player = do 
  positionPlayer <- readXY board player
  if player == 'O'
    then let x = fst positionPlayer
             y = snd positionPlayer
             new = mark x y board 1 in putStrLn(boardToStr playerToChar new)
    else let x = fst positionPlayer
             y = snd positionPlayer
             new = mark x y board 2 in putStrLn(boardToStr playerToChar new)
  if player == 'O'
    then playingGame board mkPlayer
    else playingGame board mkOpponent
  --bd <- mark x y bd 'O'
  --putStrLn (boardToStr playerToChar bd)