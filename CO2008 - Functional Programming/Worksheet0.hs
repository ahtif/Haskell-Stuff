--------------------------------------------------------------------
-- CO 2008  Functional Programming  
-- Created: January 2016, University of Leicester, UK                        
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--                       
--------------------------------------------------------------------           
-- Student Name
-- Student Number
--------------------------------------------------------------------

module Worksheet0 where 
import Char
import Data.List
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
allEqual :: Int -> Int -> Int -> Bool
allEqual m n k =  if(m == n && n == k) then True else False




--------------------------------------------------------------------
-- Exercise 10
--------------------------------------------------------------------


message :: Float -> String
message x = "The number is "++(show x)


--------------------------------------------------------------------
-- Exercise 11
--------------------------------------------------------------------

--Write a function blankVowel that given a character x returns a blank
--when x is a vowel (ie. x â^^ {a, e, i, o, u, A, . . .}) and other wise returns x.

blankVowel :: Char -> Char
blankVowel x = if(x `elem` vowels) then ' ' else x
			where vowels = ['a','e','i','o','u','A','E','I','O','U']

text :: String
text = "Altijd is Kortjakje ziek,\ndriemaal in de week,\nmaar zondags niet."

-------------------------------------------------------------------
-- Exercise 12 Challenge
--------------------------------------------------------------------

--Write a function blankVowel2 that given a character x returns a blank
--when x is a DUTCH vowel (ie. x â^^ {a, e, i, ij, o, u, A, . . .}) and other wise returns x.
-- in the Dutch language also the combination ij is treated as a vowel!

blankVowel2 :: String -> String
blankVowel2 x =
| head x `elem` vowels1 = cons head x ++ [] 
| blankVowel2 (tail x)
	where vowels1 = ["a","e","i","o","u","A","E","I","O","U","ij","IJ"]

