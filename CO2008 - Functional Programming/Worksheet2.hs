--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: February 2016, University of Leicester, UK
-- handin 14.00 hr on 13th February (summatively assessed) 
--------------------------------------------------------------------
-- Ahtif Anwar
-- 149022888
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
--


module Worksheet2 where
import Char

----------------------------------------------------------------------
-- Exercise 1: A phone book
---------------------------------------------------------------------


type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]

-- Part a)

add :: Person -> PhoneBook -> PhoneBook
add person book = person : book

-- Part b)

delete :: Name -> PhoneBook -> PhoneBook
delete name book = [(x,y) | (x,y) <- book, x/=name]

-- Part c)

find :: Name -> PhoneBook -> [PhoneNumber]
find name book = [num | (n,num) <- book, n == name]


-- Part d)
updateNum :: Person -> Person -> PhoneNumber -> Person
updateNum first second num = if first == second
							then (fst first, num)
							else first

update :: Name -> PhoneNumber -> PhoneNumber -> PhoneBook -> PhoneBook
update name oldNum newNum book = [updateNum (x,y) (name,oldNum) newNum | (x,y) <- book]

-----------------------------------------------------------------
-- Exercise 2:  Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer  = (NI,Age, Balance)
type Bank = [Customer]

-- Part a)

retired :: Customer -> Bool
retired (ni,age,balance) = age >= 60

-- Part b)

deposit :: Customer -> Float -> Customer
deposit (ni,age,balance) amt = (ni, age, balance+amt)

-- Part c)

withdraw :: Customer -> Float -> Customer
withdraw (ni,age,balance) amt = if (balance - amt) >= 0 
								then (ni,age,balance - amt)
								else (ni,age,balance)

-- Part d)

credit :: Bank -> [Customer]
credit bank = [(ni,age,balance) | (ni,age,balance) <- bank, balance >= 0]

-----------------------------------------------------------------
-- Exercise 3: 
-----------------------------------------------------------------
isOdd :: Int -> Bool
isOdd x = (x `mod` 2 == 1)

cubeOdds :: [Int] -> [Int]
cubeOdds ints = [x*x*x | x <- ints, (isOdd x)]

cubeOdds2 :: [Int] -> [Int]
cubeOdds2 ints = map cube (filter isOdd ints)
				where cube x = x*x*x

-----------------------------------------------------------------
-- Exercise 4: 
-----------------------------------------------------------------

change :: (Char, Char) -> Char -> Char
change (a,b) x = if(x == a) then b
			 else x

repChar :: (Char, Char) -> String -> String
repChar (a,b) str = [change (a,b) x | x <- str]

-----------------------------------------------------------------
-- Exercise 5a: 
-----------------------------------------------------------------

zap :: [Int] -> [Int] -> [(Int,Int)]
zap [] [] = []
zap [] (y:ys) = []
zap (x:xs) [] = []
zap (x:xs) (y:ys) = (x,y) : zap xs ys

-----------------------------------------------------------------
-- Exercise 5b:  
-----------------------------------------------------------------

addIndex :: [Int] -> [(Int,Int)]
addIndex ints = [(z,ints!!y) | y <- [0..length ints-1], z<-[1..length ints], y+1==z]

-----------------------------------------------------------------
-- Exercise 5c:  
-----------------------------------------------------------------

extend :: Int -> String -> String
extend i str = str ++ [' ' | x<-[0..(i - length str)]]

-----------------------------------------------------------------
-- Exercise 5d:  
-----------------------------------------------------------------
list :: [Int]
list = [56,49,45,43,43,42,39,39,34,33,31,27,26,24,23,22,21,20,19,17]

drawStar :: Int -> String
drawStar 0 = ""
drawStar x = if (x `mod` 3 == 0) then " *" ++ drawStar (x-1)
				else "*" ++ drawStar (x-1)

table :: [Int] -> String
table [] = [] 
table (x:xs) = "\n" ++ extend 3 (show(length list-head([length xs .. length list]))) ++ reverse(drawStar x) ++ table xs

--putStr (table list)

{-|
1  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** **
2  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *
3  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
4  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *
5  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *
6  *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
7  *** *** *** *** *** *** *** *** *** *** *** *** *** 
8  *** *** *** *** *** *** *** *** *** *** *** *** *** 
9  *** *** *** *** *** *** *** *** *** *** *** *
10 *** *** *** *** *** *** *** *** *** *** *** 
11 *** *** *** *** *** *** *** *** *** *** *
12 *** *** *** *** *** *** *** *** *** 
13 *** *** *** *** *** *** *** *** **
14 *** *** *** *** *** *** *** *** 
15 *** *** *** *** *** *** *** **
16 *** *** *** *** *** *** *** *
17 *** *** *** *** *** *** *** 
18 *** *** *** *** *** *** **
19 *** *** *** *** *** *** *
20 *** *** *** *** *** **
-}
