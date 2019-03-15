
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
              deriving (Eq, Ord, Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)

type Card  = (Value, Suite)
--Print cards of the form "VS", i.e. the two of hearts (or (Two, hearts)) becomes "2H"
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

shuffle :: [Int] -> [a] -> Error [a]
shuffle [] list = Ok list
shuffle (x:xs) list = case split x list of
    Fail -> Fail
    Ok (fHalf, sHalf) -> shuffle xs (interleave fHalf sHalf)


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
add a [] (ND) = Ok (Data a)
add a ((L):xs) (Branch left right) =    case add a xs left of                   --data to be put on the left, get the tree on the left after add is complete
                                        Fail -> Fail                            --if it's a fail, we fail
                                        Ok newLeft -> Ok (Branch newLeft right) --otherwise extract the data from the Error type and re-wrap it

add a ((R):xs) (Branch left right) =    case add a xs right of                  --as above but we're going right
                                        Fail -> Fail
                                        Ok newRight -> Ok (Branch left newRight)
add _ _ _ = Fail    --if we didn't fit any of the other clauses, we tried to add data to a node containing data or in a position that doesn't exist


--- Part c)

findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]
findpath _ _ (ND)   = []            --if the tree is a node of no data, there is no path to y such that fun y = x

findpath fun x (Data y)             --if the tree is just a node with data, apply the function to it and compare to input
    | fun y == x    = [[]]          --if its a match return a list containing the empty path as this points to the tree itself
    | otherwise     = []            --otherwise return a list of no paths (the data does not match criteria)

findpath fun x (Branch left right) = (map ((L):) resLeft) ++ (map ((R):) resRight)  --add L to all paths that came from the left side and R to those from the right (as we went these directions to get to the subtrees
    where
        resLeft =   (findpath fun x left)   --execute this function again on left side
        resRight =  (findpath fun x right)  --and the right


tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4


--Don't forget your name

-- please take care that your solution compiles.
-- of course if things don't work, you can comment them out
-- and explain in the comment that that something is wrong with it.
