--------------------------------------------------------------------
-- CO 2008  Functional Programming                           
-- Created: February 2016, University of Leicester, UK                        
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--
-------------------------------------------------------------------          
-- Ahtif Anwar
-- 149022888
--------------------------------------------------------------------


module Worksheet1 where 
import Char

-----------------------------------------------------------------
-- Exercise 1 
-----------------------------------------------------------------

type Verb  = String

pastTense :: Verb -> Verb
pastTense  v = v ++ "ed"

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------


percentof :: Float  -> Float -> Float
percentof p q = (p/100)*q


mytotalbill :: String 
mytotalbill = "The total bill amounts to "++ show (percentof 112.5 40)++ " pounds"



----------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------

type NumberOfCars = Int
type DailyCost  = Float

cost :: NumberOfCars  -> DailyCost
cost n = if (n>=0 && n<=500) then
		fromIntegral(5*n + 1000)
	else
		fromIntegral(10*n + 450)
  
---------------------------------------------------------------------
-- Exercise 4. 
---------------------------------------------------------------------

--examples:
--age 10 = 6
--age 80 = 36

type Year = Int

age :: Year -> Int
age x 
 | x>16 = (100 - x)+16
 | x<16 = 16 - x

---------------------------------------------------------------------
-- Exercise 5.  Pounds Euros
---------------------------------------------------------------------

type Euros = Float
type Pounds = Float

eurocurrency = 1.32 :: Float

p2e  :: Pounds -> Euros
p2e p = eurocurrency * p

e2p :: Euros -> Pounds
e2p e = e / eurocurrency

---------------------------------------------------------------------
-- Exercise 6  escaping rules
---------------------------------------------------------------------

rawtext :: String
rawtext = "This is a \\ \\long string,\n\ 
			\\\ \\ spanning multiple lines,\n\
			\in fact 3 lines!"

---------------------------------------------------------------------
-- Exercise 7  removeZeroes
---------------------------------------------------------------------

removeZeroes :: [Int] -> [Int]
removeZeroes n = filter isNotZero n
				where isNotZero x = (x/=0)

---------------------------------------------------------------------
-- Exercise 8.  capslockon
---------------------------------------------------------------------

capslockson :: String -> String
capslockson str = map caps str
				where caps x = if (x>='a' && x<='z') then
									toUpper x
								else
									toLower x
--------------------------------------------------------------------
-- Exercise 9.  number of charachters in Char
---------------------------------------------------------------------

listOfAllCharacters :: String
listOfAllCharacters =  map chr [33..126]

---------------------------------------------------------------------
-- Exercise 10.  removeZeroes2
---------------------------------------------------------------------

--example
--removeZeroes2 1020304 = 1234
--removeZeroes2 0 = "input should not be 0"

removeZeroes2 :: Int -> Int
removeZeroes2 int 
	| int /=0 = read (filter isNotZero (show int))
	| otherwise = error "input is 0"
				where isNotZero x = (x/='0')
