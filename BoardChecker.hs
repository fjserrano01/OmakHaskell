module BoardChecker where
 import Prelude
 --Checks if someone has won
 isWinBy [] = 0
 isWinBy bd
  | rowcheck bd == 1 = 1
  | rowcheck bd == 2 = 2
  | rowcheck bd == 0 = 0
  | diagonalcheck bd == 1 = 1
  | diagonalcheck bd == 2 = 2
  | diagonalcheck bd == 0 = 0
   
 --checks the rows of a 2d array for a win
 rowcheck [] = 0
 rowcheck (h : t)
  | rowCheckerWin h == 2 = 2
  | rowCheckerWin h == 1 = 1
  | rowCheckerWin h == 0 = rowcheck t

 --Checks if there is a diagonal win in the 2d array
 diagonalcheck [] = 0
 diagonalcheck list
  | rowcheck d == 1 = 1
  | rowcheck d == 2 = 2
  | rowcheck d == 0 = 0
   where d = diagonals list

 --Returns a 2d list of diagonal positions
 diagonals [] = repeat []
 diagonals (h:t) = takeWhile (not . null) $
    zipWith (++) (map (:[]) h ++ repeat [])
                 ([]:diagonals t)

--checks 1d array list
 rowCheckerWin [] = 0
 rowCheckerWin list
  | length (getDuplicate1 list) >= 5 = 1
  | length (getDuplicate2 list) >= 5 = 2
  | otherwise = 0

--checks if the array has a win row of player 1
 getDuplicate1 [] = []
 getDuplicate1 (h:t)
  | (h == 1) && (checkElement h t == True) = h : getDuplicate1 t
  | (h == 0) && (checkElement h t == True) = getDuplicate1 t
  | (h == 2) && (checkElement h t == True) = getDuplicate1 t
  | (checkElement h t == False) = getDuplicate1 t

--Checks if the array has a win row of player 2
 getDuplicate2 [] = []
 getDuplicate2 (h:t)
  | (h == 1) && (checkElement h t == True) = getDuplicate2 t
  | (h == 0) && (checkElement h t == True) = getDuplicate2 t
  | (h == 2) && (checkElement h t == True) = h : getDuplicate2 t
  | (checkElement h t == False) = getDuplicate2 t

-- Returns if the next element in the list is equal to the current
 checkElement _ [] = True
 checkElement x (h:t) = if x == h then True else False