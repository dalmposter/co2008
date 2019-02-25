--------------------------------------------------------------------
-- CO2008  Functional Programming                            
-- Created: Feb 2019, University of Leicester, UK                        
--------------------------------------------------------------------           
-- Student Name
-- Student Number
-- Student Login name
--------------------------------------------------------------------

--These question can and should be answered using all the functions as mentioned on the
--slides. Don't borrow library functions from the web. If in doubt ask the lecturers or TAs.
--After all the point is to teach you how to write this kind of code...

module Worksheet3 where 
import Data.Char


----------------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------------

--remove every nth element from list (where n = 1 return empty list)

skipall :: Int -> [a] -> [a]
skipall 1 _ = []
skipall _ [] = []
skipall n list
    | n <= 0    = error "Error in skipall: n cannot be less than 1"
    | otherwise = (take (n - 1) list) ++ (skipall n (drop n list))

----------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------

--First write homerge that merges two sorted lists into a sorted list

homerge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
homerge fun list [] = list;
homerge fun [] list = list;
homerge fun (x:xs) (y:ys)
    | fun x < fun y = x : (homerge fun xs (y:ys))
    | otherwise     = y : (homerge fun (x:xs) ys)

   
--Now write the higher order merge sort

hoMergeSort :: Ord b => (a -> b)  -> [a] -> [a]
hoMergeSort _ [] = []
hoMergeSort _ [x] = [x]
hoMergeSort fun xs = homerge fun (hoMergeSort fun ys) (hoMergeSort fun ws)
    where (ys,ws) = (take l xs, drop l xs)
          l = length xs `div` 2


----------------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------------


type Lastname = String 
type Username = String 
type Mark = Int

type Spreadsheet = [(Lastname, Username, Mark)]


sortLastname :: Spreadsheet ->  Spreadsheet
sortLastname sheet = hoMergeSort getName sheet
    where getName (lname, uname, mark) = lname

sortUsername :: Spreadsheet ->  Spreadsheet
sortUsername sheet = hoMergeSort getUser sheet
    where getUser (lname, uname, mark) = uname

sortMark :: Spreadsheet ->  Spreadsheet
sortMark sheet = hoMergeSort getMark sheet
    where getMark (lname, uname, mark) = mark



----------------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------


smallest :: Ord a => [a] -> a
smallest []  = error "Error in smallest: no smallest of empty list"
smallest [x] = x
smallest (x:y:sx)
    | x < y     = smallest (x:sx)
    | otherwise = smallest (y:sx)

delete :: Ord a => a -> [a] -> [a]
delete _ [] = []
delete d (x:xs)
    | x == d    = xs
    | otherwise = x:(delete d xs)

bucketsort :: Ord a => [a] ->  [a]
bucketsort [x] = [x]
bucketsort list = small:(bucketsort (delete small list))
    where small = smallest list


----------------------------------------------------------------------
-- Exercise 5
---------------------------------------------------------------------

--Follow the instructions...

-- Let a tile be a list of usually equally long strings of characters

type Tile = [String]

-- Part a)


makeTile :: Char -> Int -> [String]
makeTile x width = [line | y <- [1..height]]
    where line = [x | i <- [1..width]]
          height = width `div` 2


-- Part b)

printTile :: Tile -> IO()
printTile tile = putStr(tile2string (tile))


-- here tile2 string should convert the tile ["****","****"]
-- into the string "\n****\n****\n" (remember the newline character!)
-- tile2string ["****","****"] = "\n****\n****\n"

tile2string :: [String] -> String
tile2string []      = "\n"
tile2string (x:xs)  = "\n" ++ x ++ (tile2string xs)


-- Part c)

-- write a function vglue that glues two tiles vericall like
--           &&&      ***             &&&
-- gluing of &&& and  *** should give &&&
--                                    ***
--                                    ***
 
vglue :: Tile -> Tile -> Tile
vglue t1 t2 = t1 ++ t2



-- Part d)

-- next write a function hglue that glues two tiles horizontally like
--           ***      ***             ******
-- gluing of *&* and  *** should give *&****
--           ***      ***             ******

hglue :: Tile -> Tile -> Tile
hglue [] [] = []
hglue x []  = x -- I don't think these 2 cases are necessary
hglue [] x  = x -- here just in case
hglue (x:xs) (y:ys) = (x++y):(hglue xs ys)

-- Part e)

-- Next we want to print chessboards
-- so we introduce a type of boards in the form 
-- of a list of a list of tiles.

type Board = [[Tile]]

--Next function: delete comments after writing board2tile
printBoard :: Board -> IO()
printBoard board = printTile (board2tile board )

-- to print a board we first glue all its tiles together
-- using a function board2tile :: Board -> Tile
-- we need to help functions
--
-- col2tile  will glue a column of tiles vertically to a tile

col2tile :: [Tile]->Tile
col2tile [x] = x
col2tile (x:xs) = vglue x (col2tile xs)



-- row2tile  will glue a row of tiles horizontally to a tile

row2tile :: [Tile]->Tile
row2tile [x] = x
row2tile (x:xs) = hglue x (row2tile xs)


-- Part f)

-- So, if we think of a board as a column of rows of tiles,
-- then we can convert a board into a tile using  
-- col2tile and row2tile

board2tile :: Board ->Tile
board2tile board = col2tile (map row2tile board) 


-- Part g)

-- if we can now make a function that "prints" an adge around a tile,
--  we can print boards with an edge.

printBoardWithEdge :: Board -> IO()
printBoardWithEdge board = printTile (edge (board2tile board ))



-- here the function edge :: Tile -> Tile should
-- produces a border around a rectangular tile:
-- .----.
-- |****|
-- |****|
-- .----.
-- edge (makeTile '*' 4) = [".----.","|****|","|****|",".----."]

edge :: Tile -> Tile
edge (t:ts) = [top] ++ map addSide (t:ts) ++ [top]
    where addSide x = "|" ++ x ++ "|"
          top       = "." ++ ['-'| y <- [1..n]] ++ "."
          n         = length t

-- Part h)

-- Finally: write a function that creates an nxn chessboard of 
-- "white" tiles of the form makeTile ' ' n
-- "black" tiles of the form makeTile '*' n
-- surrounded by a nice edge and
-- such that the square at the bottom left is black
-- EG chessboard 3 should give a tile that prints like
-- .----------------.
-- |    ****    ****|
-- |    ****    ****|
-- |****    ****    |
-- |****    ****    |
-- |    ****    ****|
-- |    ****    ****|
-- |****    ****    |
-- |****    ****    |
-- .----------------.

chessboard :: Int -> Board
chessboard n = map getRow [1..n]
    where   getRow x = if x `mod` 2 == 1 then black else white
                -- black indicates a row of n length starting with a black tile
            black = map getTile [0..(n-1)]
            white = map getTile [1..n]
            getTile y = if y `mod` 2 == 0 then makeTile '*' n else makeTile ' ' n
            


-- with the following function you can print such boards:

chess :: Int -> IO()
chess n = printBoardWithEdge (chessboard n)


