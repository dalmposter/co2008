
--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: February 2019, University of Leicester, UK
-- handin 17.00 hr on Sunday 17st February (assessed) 
--------------------------------------------------------------------
-- Student Name 	Dominic Cousins
-- Student Number	179003035
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
-- use a good looking layout

module Worksheet2 where
import Data.Char

----------------------------------------------------------------------
-- Exercise 1: A phone book
---------------------------------------------------------------------

type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]

testBook = [("Dominic", 12345), ("Harry", 67890), ("Max", 13579)] :: PhoneBook

-- Part a)

add :: Person -> PhoneBook -> PhoneBook
add p b = p : b

-- Part b)

delete  :: Name -> PhoneBook -> PhoneBook
delete name book = filter notName book
    where notName (x,y) = x /= name

--  Part c)

find  :: Name -> PhoneBook -> [PhoneNumber]
find name book = [number | (nom, number) <- (filter isName book)]
    where isName (x,y) = x == name

--  Part d)

update :: Name -> PhoneNumber -> PhoneNumber -> PhoneBook -> PhoneBook
update name oldNum newNum bookIn
    | bookIn == []                      = []
    | head testBook == (name, oldNum)   = (name, newNum) : tail testBook
    | otherwise                         = head testBook : update name oldNum newNum (tail bookIn)

-----------------------------------------------------------------
-- Exercise 2:  Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer = (NI,Age, Balance)
type Bank = [Customer]

testBank = [(0, 20, 100.0), (1, 70, 999.0), (2, 54, 60000.0), (3, 19, -50.0)] :: Bank

-- Part a)
retired :: Customer -> Bool
retired (ni, age, bal) = (age >= 67)

-- Part b)
deposit :: Customer -> Float -> Customer
deposit (ni, age, bal) toAdd = (ni, age, bal + toAdd)

-- Part c)
withdraw :: Customer -> Float -> Customer
withdraw (ni, age, bal) toTake
    | (bal - toTake) < 0    = (ni, age, bal)
    | otherwise             = (ni, age, bal - toTake)

-- Part d)
credit :: Bank -> [Customer]
credit bank = filter notOverdrawn bank
    where notOverdrawn (ni, age, bal) = (bal >= 0)


-----------------------------------------------------------------
-- Exercise 3: cubeOdds
-----------------------------------------------------------------

isOdd :: Int -> Bool
isOdd num = (mod num 2) == 1

cubeOdds :: [Int]-> [Int]
cubeOdds listIn = [x*x*x | x <- listIn, isOdd x]

cubeOdds2 :: [Int]-> [Int]
cubeOdds2 listIn = map cube (filter isOdd listIn)
    where cube x = x*x*x


-----------------------------------------------------------------
-- Exercise 4. addIndex
-----------------------------------------------------------------

addIndex :: [Int] -> [(Int,Int)]
addIndex listIn = addIndexRec listIn 1

addIndexRec :: [Int] -> Int -> [(Int, Int)]
addIndexRec listIn index
    | listIn == []  = []
    | otherwise     = (index, head listIn) : addIndexRec (tail listIn) (index + 1)










