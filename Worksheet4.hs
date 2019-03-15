
-- CO 2008 Functional Programming 
-- Created: March 2019, University of Leicester, UK 
-------------------------------------------------------------------- 
-- Student Name     Dominic Cousins
-- Student Number   179003035
--------------------------------------------------------------------
--
-- Please don't use the internet or your friends; 
-- instead consult the slides.
-- On the slides you find explanantion regarding 
-- Trees and Error type.
-- this should be your own work
--

module Worksheet4 where
---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------
data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K|A
             deriving (Eq, Ord, Enum)
instance Show Value where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show J = "J"
    show Q  = "Q"
    show K = "K"
    show A = "A"

data Suite = Hearts | Spades | Diamonds | Clubs
             deriving (Eq, Ord, Enum)
instance Show Suite where
    show Hearts = "H"
    show Spades = "S"
    show Diamonds = "D"
    show Clubs = "C"
    
data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)

type Card  = (Value, Suite)
--Couldn't get this function working unfortunately

myShow :: Card -> String
myShow (v, s) = (show v)++(show s)

--- Part c)
pack :: [Card]
pack = [(x, y) | y <- [(Hearts)..(Clubs)], x <- [(Two)..(A)]]

--- Part d)

colour :: Card -> Colour
colour (_, s)
    | s == Hearts || s == Diamonds  = Red
    | otherwise                     = Black


--- Part e)

split :: Int -> [a] -> (Error ([a],[a]))
split n list
    | n > length list || n < 0 = Fail
    | otherwise = Ok (take n list, drop n list)


interleave ::  [a] ->  [a] -> [a]
interleave list [] = list
interleave [] list = list
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)


--- Part f)

--shuffle :: [Int] -> [a] -> Error [a]
--shuffle [] list = Ok list
--shuffle (x:xs) list
--    | split x list == Fail = Fail
--    | otherwise = shuffle xs (interleave fHalf sHalf)
--        where Ok (fHalf, sHalf) = split x list


---------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------


data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq)

data Dir = L | R 
           deriving (Show,Eq)

type Path =  [Dir] 
    
--- Part a)

extract :: Path  -> Btree a -> Error a
extract [] (Data a) = Ok a          --if the Dir list is empty and the tree is a data node, return the data in the node
extract [] _ = Fail                 --if the Dir list is empty and the tree is anything else, return Fail
extract (x:xs) (Branch left right)  --if we have at lease one Dir in the list and the tree starts with a branch, enter the branch and recur
    | x == L = extract xs left
    | x == R = extract xs right
extract (x:xs) _ = Fail             --if we reached this clause we cannot perform a valid computation on the inputs


--- Part b)

add :: a -> Path -> Btree a -> Error (Btree a)
add a [] (ND) = (Data a)
add a (x:xs) (Branch left right)
    | x == L = (Branch (add a xs left) right)
    | x == R = (Branch left (add a xs right))


--- Part c)

--findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]


tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4


--Don't forget your name

-- please take care that your solution compiles.
-- of course if things don't work, you can comment them out
-- and explain in the comment that that something is wrong with it.
