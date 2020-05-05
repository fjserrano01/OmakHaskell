module UiOmokGame where 
 import System.IO
 import System.Random
 import System.Exit
 import BoardElements
 import StoneandMovement
  --For reading user input
 playerToChar p =
  case p of
  1 -> 'O'
  2 -> 'X'
  x -> '.'
 

 
 readXY bd p = do
  putStrLn "Enter Position: "
  line <- fmap (map read.words) getLine
  let parsed = line in do
    if length parsed == 2 then do
      let x = head parsed
          new = tail parsed
          y = head new in 
           if x == -1
              then return (x, y)
              else 
                if y == -1 
                  then return (x, y)
                  else 
                    if isEmpty x y bd 
                      then return (x, y)
            else readXY bd p
      else 
        readXY bd p




 playingGame bd player = do 
  if isFull bd == True
    then die "Thank you for playing"
  else do
   positionPlayer <- readXY bd player
   let x = fst positionPlayer
       y = snd positionPlayer in
        if x == -1
         then die "Thank you for playing"
         else 
           if y == -1
            then die "Thank you for playing"
            else
              if player == mkPlayer
                then let x = fst positionPlayer
                         y = snd positionPlayer
                         new = mark x y bd 1 in do putStrLn(boardToStr playerToChar new)
                                                   playingGame new mkOpponent
                else let x = fst positionPlayer
                         y = snd positionPlayer
                         new = mark x y bd 2 in do putStrLn(boardToStr playerToChar new)
                                                   playingGame new mkPlayer
  
  --bd <- mark x y bd 'O'
  --putStrLn (boardToStr playerToChar bd)