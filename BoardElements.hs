--lab Haskell, make board and handle getting row or col of board.
module BoardElements where
 --make nxn board for the game
 --Does not take zero value, make row then populate with another column of size n
 -- 0 will be used as empty value
 mkBoard 0 = []
 mkBoard x = take x (mkBoardRow x : mkBoard x)
 mkBoardRow 0 = []
 mkBoardRow x = 0 : mkBoardRow (x-1)

 --mkPlayer create and return the first player
 --mkPlayer is 1 -- mkOpponent is 2 in the list
 mkPlayer = 'O'
 mkOpponent = 'X'

--to get size of the board if its nxn
 size [] = 0
 size (h : t) = size(t) + 1

-- starts at 0
--To get the row of the board
--concatenates the number at position x in the first col then each col after
 row _ [] = []
 row x (h:t) = col : row x t where
  col = h !! x

-- starts at 0
--to get column of the board
 column _ [] = []
 column x (h:t) = (h:t) !! x

--marks position of the array as the player. starts with 1
 markRow 0 (h : t) p = p : t
 markRow n (h : t) p = h : markRow (n-1) t p