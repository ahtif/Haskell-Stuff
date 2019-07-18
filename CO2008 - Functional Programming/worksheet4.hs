
-- CO 2008 Functional Programming 
-- Created: March 2016, University of Leicester, UK 
-------------------------------------------------------------------- 
-- Ahtif Anwar
-- 149022888 
--------------------------------------------------------------------
--
-- VERY IMPORTANT: call this file worksheet4.hs when you hand in
-- starting with small "w"
-- if you don't call it EXACTLY LIKE THAT the hand in system won't accept your file

module Worksheet4 where 

---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------
data Value = A|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K
             deriving (Eq, Ord, Enum)
instance Show Value
--- Part a)
 where 
  show A     = "A"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show J     = "J"
  show Q     = "Q"
  show K     = "K"

data Suite = Hearts | Spades | Clubs | Diamonds
             deriving (Eq, Ord, Enum)
instance Show Suite
--- Part b)
 where 
  show Hearts   = "H"
  show Spades   = "S"
  show Clubs    = "C"
  show Diamonds = "D"

data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)

type Card = (Value, Suite)

-- Part c (unassesed)
showCard :: Card -> String
showCard (x,y) = show x ++ show y

--- Part d)

pack :: [Card]
pack = [(x,y) | x <- [A .. K], y <- [Hearts .. Diamonds]]

--- Part e)

colour :: Card -> Colour
colour (x,y) = if y == Diamonds || y == Hearts
               then Red 
               else Black


--- Part f)

split :: Int -> [a] -> (Error ([a],[a]))
split n xs
 | n < 0          = Fail
 | n > length xs  = Fail
 | n <= length xs = Ok (take n xs, drop n xs)

interleave :: [a] -> [a] -> [a]
interleave xs []         = xs
interleave [] ys         = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys


--- Part g)

standard :: [Int]
standard = [23,26,25,31,19,27]

shuffle :: [Int] -> [a] -> Error [a]
shuffle [] list = Ok list
shuffle (x:xs) list
 | x < 0 = Fail
 | x >= 0 = shuffle xs (interleave first second)
        where Ok (first, second) = split x list 

---------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------


data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq) 

data Dir = L | R 
           deriving (Show,Eq)

type Path = [Dir] 
    
--- Part a)

extract :: Path -> Btree a -> Error a
extract [] ND                  = Fail
extract [] (Branch ND ND)      = Fail
extract [] (Branch left right) = Fail
extract (x:xs) ND              = Fail
extract (x:xs) (Data a)        = Fail
extract [] (Data a)            = Ok a
extract (x:xs) (Branch leftTree rightTree)
 | x == L = extract xs leftTree
 | x == R = extract xs rightTree

--- Part b)

add :: a -> Path -> Btree a -> Error (Btree a)
add d [] (Branch left right) = Fail
add d [] (Data a)            = Fail
add d [] ND                  = Ok (Data d)
add d (x:xs) (Data a)        = Fail
add d (x:xs) ND
 | x == L = Ok (Branch (Data d) ND)
 | x == R = Ok (Branch ND (Data d))
add d (x:xs) (Branch left right)
 | x == L = case (add d xs left) of
              Ok (Data d) -> Ok (Branch (Data d) right)
              Ok (newLeft) -> Ok (Branch newLeft right)
              Fail -> Fail
 | x == R = case (add d xs right) of
              Ok (Data d) -> Ok (Branch left (Data d))
              Ok (newRight) -> Ok (Branch left newRight)
              Fail -> Fail
      where Ok newRight = add d xs ND
            Ok newLeft  = add d xs ND

--- Part c)

findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]
findpath f x tree = findAllPaths f x tree []

findAllPaths :: Eq b => (a -> b) -> b -> Btree a -> Path -> [Path]
findAllPaths f x ND path = []
findAllPaths f x (Data d) path
 | f d == x  = [path++[]]
 | otherwise = []
findAllPaths f x (Branch left right) path = leftList ++ rightList
    where leftList  = (findAllPaths f x left (path++[L]))
          rightList = (findAllPaths f x right (path++[R]))


tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4



---------------------------------------------------------------------
----- EXERCISE 3 Family tree question
---------------------------------------------------------------------

-- a

{-- 
deriving Show in the Tree declaration allows you to convert the Tree to a String
which allows you to output it in the console. 
The Person type does not need a deriving show declaration as it is already of type String.
--}

-- b
{-- 
The function putStr has type putStr :: String -> IO ().
The term putStr "Hello" takes the string "Hello" and outputs it to the console.
--}

--c

sort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sort ord []     = []
sort ord (x:xs) = sort ord less ++ occs ++ sort ord more
              where less = [e | e <- xs, ord e x]
                    occs = x :  [e | e <- xs, e == x]
                    more = [e | e <- xs, ord x e]

-- d

data Tree a = U | F a (Tree a) (Tree a) deriving Show

term = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))
type Person = String

genlabel :: (Tree Person) -> (Tree (Int,Person))
genlabel fTree = addLabel 1 fTree

addLabel :: Int -> (Tree Person) -> (Tree (Int,Person))
addLabel n U = U
addLabel n (F str left right) = (F (n,str) leftT rightT)
                            where leftT  = addLabel (2*n) left
                                  rightT = addLabel (2*n+1) right

--e

type Tile = [String]

flatten :: Tile -> String 
flatten []     = []
flatten (x:xs) = x ++ flatten xs

preprint ::  (Int,Person) -> String
preprint (num, person) = show num ++ ":  " ++ person

sortPerson :: (Int,Person) -> (Int,Person) -> Bool
sortPerson (num, person) (num1, person1) = if num <= num1
                                           then True
                                           else False

flattenTree ::  (Tree (Int,Person)) -> [(Int,Person)]
flattenTree U                            = []
flattenTree (F (num, person) left right) = (num, person):((flattenTree left) ++ (flattenTree right))

printTree :: [(Int,Person)] -> String
printTree []     = []
printTree (x:xs) = preprint x ++ "\n" ++ printTree xs

printlist :: Tree Person -> IO()
printlist fTree = putStr (printTree (sort (sortPerson) (flattenTree (genlabel fTree))))


tree = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))

test1 = printlist tree

--f

flattenTree2 ::  (Tree (Int,Person)) -> [(Int,Person)]
flattenTree2 U                            = []
flattenTree2 (F (num, person) left right) = ((flattenTree2 left) ++ [(num, person)] ++ (flattenTree2 right))

printTabs :: Int -> String
printTabs 1 = []
printTabs n = printTabs (n `div` 2) ++ "\t"

print2D :: [(Int,Person)] -> String
print2D []                = []
print2D ((int,person):xs) = printTabs int ++ person ++ "\n" ++ print2D xs

print2Dtree :: Tree Person -> IO()
print2Dtree fTree = putStr (print2D (flattenTree2 (genlabel fTree)))

test2 = print2Dtree tree


--Don't forget your name

-- please take care that your solution compiles.
-- of course if things don't work, you can comment them out
-- and explain in the comment that that something is wrong with it.
