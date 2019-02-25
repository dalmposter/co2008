--------------------------------------------------------------------
-- CO 2008  Functional Programming  
-- Created: October 2019, University of Leicester, UK                        
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--                       
--------------------------------------------------------------------           
-- Student Name		Dominic Cousins
-- Student Number	179003035
--------------------------------------------------------------------

module Worksheet0 where 
import Data.Char

--------------------------------------------------------------------
-- Exercise 6

--------------------------------------------------------------------

myint :: Int
myint = 707


myfloat :: Float
myfloat = 12.5


mychar :: Char 
mychar = 't'


mystring :: String
mystring = "Hello "

less :: Bool
less = (myint < 100)


cube :: Int -> Int
cube n = n*n*n

--------------------------------------------------------------------
-- Exercise 7
--------------------------------------------------------------------


-- A function with two integer input that adds them.
plus :: Int -> Int -> Int 
plus m n = m + n


-- A function with three integer inputs and Boolean output;
-- yields True if all inputs equal, else False.
--allEqual :: Int -> Int -> Int -> Bool
allEqual m n k = m == n && n == k

--------------------------------------------------------------------
-- Exercise 10
--------------------------------------------------------------------

-- A function that gives the message "The number is XXX"

message :: Float  -> String
message x = "The number is "++(show x)



--------------------------------------------------------------------
-- Exercise 11
--------------------------------------------------------------------

-- If x is a vowel return blank, else return x

vowels :: String
vowels = "aeiouAEIOU"

blankVowel :: Char -> Char
blankVowel x = blankVowelRec x vowels

blankVowelRec :: Char -> String -> Char
blankVowelRec x r
    | x==(head r)   = ' '
    | (tail r)==""  = x
    | otherwise     = blankVowelRec x (tail r)


--------------------------------------------------------------------
-- Exercise 12
--------------------------------------------------------------------

n :: Int
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


{- Note the above function definition is surrounded by comment indicators.
Please remove them,
and reload the worksheet
(use command :r in GHCi) -}

