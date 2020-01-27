Connect 4

----------------------------------------------------------------------

> import Data.List
> import Data.Char
> import System.IO

----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

> test :: Board
> test = [[B,B,B,B,B,B,B], [B,B,B,B,B,B,B], [B,B,B,B,B,B,B], [B,B,B,X,X,B,B], [B,B,O,O,X,B,B], [B,O,O,X,X,X,O]]

showBoard test
.......
.......
.......
...XX..
..OOX..
.OOXXXO
-------
0123456

Empty board:

> empty :: Board
> empty =  replicate rows (replicate cols B)

If all the entries are not blank, the board is full:

> full :: Board -> Bool
> full = all (/= B). concat

This function determines whose turn it is according to the board:

> turn :: Board -> Player
> turn b = if os <= xs then O else X
>          where
>             os = length (filter (== O) ps)
>             xs = length (filter (== X) ps)
>             ps = concat b


Define if a player has a connected line in a row
----------------------------------------------------------------------

A player "havs a row" if he has a connected line in a row.

> hasRow :: Player -> Row -> Bool
> hasRow p row = connect (map length [ps | ps <- pss, any (==p) ps])
>                where pss     = runs row
>                      connect = any (>= win)

This function splits a list of players into a list of runs of repeated players.
Example:
runs [B,B,O,O,X,B,B]
[[B,B],[O,O],[X],[B,B]]

> runs :: [Player] -> [[Player]]
> runs [] = []
> runs xs = [chomp xs] ++ runs (drop (length (chomp xs)) xs)

This function selects a run of repeated players from the start of a row,
with the run being as long as possible.
Example:
chomp [B,B,O,O,X,B,B]
[B,B]

> chomp :: Row -> [Player]
> chomp (x:xs) = x : takeWhile (== x) xs


Define if a player wins
----------------------------------------------------------------------

A player "has won" if he "has row" in at least one row or column or diagonal.

> hasWon :: Player -> Board -> Bool
> hasWon p b = any (==True) (map (hasRow p) (toRows b))
>              || any (==True) (map (hasRow p) (toCols b))
>              || any (==True) (map (hasRow p) (toDiag b ++ toDiag (map reverse b)))

> toRows :: Board -> [Row]
> toRows = id

> toCols :: Board -> [Row]
> toCols = transpose

This function selects valid diagonals (length is larger than wins) of a board and its
transposed transformation.
r: row index
r+o: column index (row index plus offset)

> toDiag :: Board -> [Row]
> toDiag b = [[b !! r !! (r+o) | r <- [0..rows-1], r+o <= cols-1]| o <- [0..cols-win]]
>            ++ [[toCols b !! r !! (r+o) | r <- [0..cols-1], r+o <= rows-1]| o <- [0..rows-win]]


Making a move
----------------------------------------------------------------------

A column is "valid" to move if the first row of the column is blank.

> valid :: Board -> Int -> Bool
> valid b i = ((head b) !! i) == B

The function applies a move to the board. If the move is "valid", return a board as the result.

> move :: Player -> Int -> Board -> Board
> move p i b = if valid b i then chop cols (xs ++ [p] ++ ys) else []
>              where (xs,B:ys) = splitAt ((findRow i b) * cols + i) (concat b)

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)

The function can find the bottom row which has B in the i column.

> findRow :: Int -> Board -> Int
> findRow i []                   = -1
> findRow i (x:xs) | x !! i == B = 1 + findRow i (xs)
>                 | otherwise   = -1


Reading a number
----------------------------------------------------------------------

Read the board index from a human player and display the prompt if read a invalid number

> getNat :: String -> IO Int
> getNat prompt = do putStr prompt
>                    xs <- getLine
>                    if xs /= [] && validNat xs then
>                       return (read xs)
>                    else
>                       do putStrLn "ERROR: Invalid number"
>                          getNat prompt

Check if the number is "valid" - in the range of board index.

> validNat :: String -> Bool
> validNat c | elem (read c) [0..cols-1] = True
>            | otherwise                 = False

> prompt :: Player -> String
> prompt p = "Player " ++ show p ++ ", enter your move: "


Game Tree
----------------------------------------------------------------------

Declare tree type.

> data Tree a = Node a [Tree a]
>               deriving Show

> gametree :: Board -> Player -> Tree Board
> gametree b p = Node b [gametree b' (next p) | b' <- moves b p]

The function builds a game tree from the starting board and player.

> moves :: Board -> Player -> [Board]
> moves b p
>    | won b     = []
>    | full b    = []
>    | otherwise = filter (/= []) [move p i b | i <- [0..cols-1]]

> won :: Board -> Bool
> won b = hasWon O b || hasWon X b

This function determines who the next player is:

> next :: Player -> Player
> next O = X
> next B = B
> next X = O


Pruning the tree
----------------------------------------------------------------------

The function prue the game tree to the depth.

> prune :: Int -> Tree a -> Tree a
> prune 0 (Node x _)  = Node x []
> prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


Minimax algorithm
----------------------------------------------------------------------

> minimax :: Tree Board -> Tree (Board, Player)
> minimax (Node b [])
>   | hasWon O b  = Node (b,O) []
>   | hasWon X b  = Node (b,X) []
>   | otherwise   = Node (b,B) []
> minimax (Node b ts)
>   | turn b == O = Node (b, minimum ps) ts'
>   | turn b == X = Node (b, maximum ps) ts'
>                   where
>                      ts' = map minimax ts
>                      ps  = [p | Node (_,p) _ <- ts']

> bestmove :: Board -> Player -> Board
> bestmove b p = head [b' | Node (b',p') _ <- ts, p' == best]
>                where
>                   tree             = prune depth (gametree b p)
>                   Node (_,best) ts = minimax tree


Human vs computer
----------------------------------------------------------------------

> main :: IO ()
> main = do hSetBuffering stdout NoBuffering
>           play empty O

> play :: Board -> Player -> IO()
> play b p = do showBoard b
>               play' b p

> play' :: Board -> Player -> IO ()
> play' b p
>   | hasWon O b = putStrLn "Player O wins!\n"
>   | hasWon X b = putStrLn "Player X wins!\n"
>   | full b     = putStrLn "It's a draw!\n"
>   | p == O     = do i <- getNat (prompt p)
>                     case move p i b of
>                        [] -> do putStrLn "ERROR: Invalid move"
>                                 play' b p
>                        b' -> play b' (next p)
>   | p == X     = do putStr "Player X is thinking... "
>                     play (bestmove b p) (next p)
