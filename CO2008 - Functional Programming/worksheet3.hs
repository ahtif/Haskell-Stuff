--------------------------------------------------------------------
-- CO2008  Functional Programming                            
-- Created: Feb 2016, University of Leicester, UK                        
--------------------------------------------------------------------           
-- Student Name
-- Student Number
-- Student Login name
--------------------------------------------------------------------

--These question can and should be answered using all the functions as mentioned on the
--slides. Don't borrow library functions from the web. If in doubt ask the lecturers or TAs.
--After all the point is to teach you hwo to write this kind of code...

module Worksheet3 where 
import Data.Char


----------------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------------


skipall :: Int -> [a] -> [a] 
skipall n list = skipall2 n 1 list 

skipall2 :: Int -> Int -> [a] -> [a] 
skipall2 n m [] = []
skipall2 0 m list = list
skipall2 n m (x:xs)
  | n < 0          = (x:xs)
  | m `mod` n == 0 = skipall2 n (m+1) xs
  | otherwise      = x : skipall2 n (m+1) xs

----------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------

compareP :: String -> String -> Bool
compareP [] [] = True
compareP (x:xs) [] = True
compareP [] (x:xs) = False
compareP (x:xs) (y:ys) = if x/=y then False else compareP xs ys


----------------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------------

--First write homerge that merges two sorted lists into a sorted list

--homerge :: Ord b => (a -> b) -> [a] -> [a] -> [a]

   
--Now write the higher order merge sort

--hoMergeSort :: Ord b => (a -> b)  -> [a] -> [a]




----------------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------


type Lastname = String 
type Username = String 
type Mark = Int

type Spreadsheet = [(Lastname, Username, Mark)]


--sortLastname :: Spreadsheet ->  Spreadsheet

--sortUsername :: Spreadsheet ->  Spreadsheet

--sortMark :: Spreadsheet ->  Spreadsheet



----------------------------------------------------------------------
-- Exercise 5
---------------------------------------------------------------------


--smallest :: Ord a => a -> [a] -> a

--delete :: Ord a => a -> [a] -> [a]

--bucketsort :: Ord a => [a] ->  [a]


----------------------------------------------------------------------
-- Exercise 6
---------------------------------------------------------------------

--display :: [String] -> IO()


--triangle :: Int -> [String]

--test = display (triangle 3)



----------------------------------------------------------------------
-- Exercise 7
---------------------------------------------------------------------


--tree :: Int -> [String]

{-display(tree 3) should result in
   *
  ***
   *
  ***
 *****
   *
  ***
 *****
*******
-}



----------------------------------------------------------------------
-- Exercise 8
---------------------------------------------------------------------


--forest :: Int -> [String]


{- display (forest 3) should give as output exactly:
             *   
            ***  
             *   
            ***  
      *    ***** 
     ***     *   
      *     ***  
 *   ***   ***** 
*** ***** *******
-}