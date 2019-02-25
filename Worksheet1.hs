--------------------------------------------------------------------
-- CO 2008  Functional Programming                           
-- Created: February 2018, University of Leicester, UK

-- Handindate  18.00 on Sunday 10/2/2019
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--
-------------------------------------------------------------------          
-- Student Name		Dominic Cousins
-- Student Number	179003035
--------------------------------------------------------------------

--
--use GHCI
--

--

module Worksheet1 where 
import Data.Char

-----------------------------------------------------------------
-- Exercise 1 
-----------------------------------------------------------------

type Verb  = String

pastTense :: Verb -> Verb
pastTense v = v++"ed"

----------------------------------------------------------------------
-- Exercise 2 
----------------------------------------------------------------------

-- Mass in kg and Height in meters
-- Would multiply result by 703 is input is in pounds and inches

type Mass = Float
type Height = Float
type BMI = Float

bmi :: Mass -> Height -> BMI
bmi m h = m / h

----------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------

type NumberOfCars = Integer
type DailyCost = Float

cost :: NumberOfCars -> DailyCost
cost n
	| n<0 		= error "Error in cost : Cars produced per day cannot be negative"
	| n<=500 	= 5 * (fromInteger n) + 1000
	| otherwise = 10 * (fromInteger n) + 450
  
---------------------------------------------------------------------
-- Exercise 4. 
---------------------------------------------------------------------

-- This year is 2018.
-- if you are born in 10 (abbreviation for 2010)
-- your age is 8.

-- if however you are born in 65 (the that must be abbreviation for 1965)
-- your age is 2018 - 1965 = 53

-- It's 2019 but sticking with the premise that it's 2018

type Year = Int

age :: Year -> Int
age n
	| n<=18 	= 18 - n
	| otherwise = 118 - n

---------------------------------------------------------------------
-- Exercise 5.  Pounds Euros
---------------------------------------------------------------------

type Euros = Float
type Pounds = Float

eurocurrency = 1.14 :: Float

p2e  :: Pounds -> Euros
p2e p = p * eurocurrency

e2p :: Euros -> Pounds
e2p e = e / eurocurrency

---------------------------------------------------------------------
-- Exercise 6.  Pounds Euros refined
---------------------------------------------------------------------

-- Rounds to 2 decimal places
twoDP :: Float -> Float
twoDP f = (fromInteger (round (f * 100))) / 100

prettyprintEuro :: Euros -> String
prettyprintEuro n = "€" ++ (show (twoDP n))

prettyprintPound :: Pounds -> String
prettyprintPound n = "£" ++ (show (twoDP n))

-- given a float, the output should be a string,
-- first symbol of which is either the euro (€) or pound sign (£) repectively.
-- Can you manage to output the first two decimals of the float?

---------------------------------------------------------------------
-- Exercise 7  escaping rules
---------------------------------------------------------------------

-- Additional \n on the end so the output doesnt hug the working directory
rawtext :: String
rawtext = "\"This is a \\ \\long string,\n\\ \\ spanning multiple lines,\nin fact 3 lines!\"\n"

-- Didnt work as type String, putStr seems to produce a type IO()
text :: IO()
text = putStr rawtext

-- uncomment the above two line, and try text in ghci window...

---------------------------------------------------------------------
-- Exercise 8  removeZeroes
---------------------------------------------------------------------

noZeroes :: Int -> Bool
noZeroes 0 = False
noZeroes _ = True

removeZeroes :: [Int] -> [Int]
removeZeroes l = filter noZeroes l

---------------------------------------------------------------------
-- Exercise 9.  capslockon
---------------------------------------------------------------------

diff=ord 'a' - ord 'A'

-- 'A'...'Z', '['...'`', 'a'...'z'

invertCase :: Char -> Char
invertCase c
	| c<'A' 	= c						-- c is before the alphabet
	| c>'z'		= c						-- c is after the alphabet
	| c<='Z'	= chr ((ord c) + diff)	-- c is uppercase
	| c<'a'		= c						-- c is between the upper and lower case letters
	| c<='z'	= chr ((ord c) - diff)	-- c is lowercase
	| otherwise = c						-- Nothing should reach here but just in case, output c

capslockon :: String -> String
capslockon s = map invertCase s

--------------------------------------------------------------------
-- Exercise 10.  number of characters in Char
---------------------------------------------------------------------

-- running this (or preferably putStr (listOfAllCharacters 0)) prints EVERY character in the system
-- the number of characters is equal to maxChar + 1, you have been WARNED

-- the highest number, n, for which chr n returns without error
maxChar=1114111 :: Int

listOfAllCharacters :: Int -> String
listOfAllCharacters n
	| n<maxChar 	= [chr n] ++ "," ++ (listOfAllCharacters (n+1))
	| otherwise		= [chr n]

-- try and inspect with
-- putStr listOfAllCharacters
-- how all these
-- characters look like
---------------------------------------------------------------------
-- Exercise 11.  removeZeroes2
---------------------------------------------------------------------

--example
--removeZeroes2 1020304 = 1234
--removeZeroes2 0 = "input should not be 0"

-- removeZeroes2 :: [Int] -> [Int]

