-- lab haskell,  handle movement and marking on board
module StoneandMovement where  
 import Prelude
 import BoardElements

 -- mark a place in the board to the player
 -- x is columns y is rows
 mark _ _ [] _ = []
 mark 0 y (h:t) p = m : t
  where m = markRow y h p
 mark x y (h:t) p = h : mark (x-1) y t p

 --checks if place in the list is empty
 --Does not handle larger x y input than list
 isEmpty x y bd
  | column x bd !! y == 0 = True
  | otherwise = False

--Checks if place in the list is marked
 isMarked x y bd
  | column x bd !! y == 1 = True
  | column x bd !! y == 2 = True
  | otherwise = False

--Checks if place is marked by a specific player
 isMarkedBy x y bd p
  | column x bd !! y == p = True
  | otherwise = False

--Returns the player that placed a stone in place (x, y)
 marker x y board
  | column x board !! y == 1 = mkPlayer
  | column x board !! y == 2 = mkOpponent
  | otherwise = '.'

--Indicates if there are any remaining empty places
 isFull [] = True
 isFull (h : t)
  | empty h == False = False
  | otherwise = isFull t

 empty [] = True
 empty (h : t)
  | h == 0 = False
  | otherwise = empty t



 boardToStr f bd = concat [ "\n " ++ (show (pos f x) ) | x <- bd ]
 --boardToStr _ [] = []
 --boardToStr f (h:t) = pos f h  : boardToStr f t

 pos _ [] = []
 pos f (h:t) = f h : ' ' : pos f t

 --isWonBy bd p = 