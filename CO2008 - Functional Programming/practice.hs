-- replace1 2 9 [32,42,45,61,23] = [32,9,45,61,23]
{-|}
replace1 :: Int -> Int -> [Int] -> [Int]
replace1 0 n (x:xs) = x:xs
replace1 i n [] = []
replace1 1 n (x:xs) = n:xs
replace1 i n (x:xs) = x : replace1 (i-1) n xs


takeWhileSmaller :: Int -> [Int] -> [Int]
takeWhileSmaller n [] = []
takeWhileSmaller n (x:xs) 
 | n > x = x : takeWhileSmaller n xs
 | otherwise = []

takes :: Int -> [Int] -> [Int]
takes n x = [a | a<-x, a<n]

map :: (a->b) -> [a] -> [b]

filter :: (a -> Bool) -> [a] -> [a]

snd :: (a,b) -> b

(>2)

f :: (Int->Int) -> Int

[Int] -> [Int]
-}

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs 

revers :: [a] -> [a]
revers [] = []
revers (x:xs) = revers xs ++ [x]


copy :: Int -> a -> [a]
copy 0 a = []
copy 1 a = [a]
copy n a = a : copy (n-1) a
--copy n a = [a | x <- [1..n] ]


coins = [1,2,5,10,20,50]
 
withcoins 1 x = [[x]]
withcoins n x = concatMap addCoin [0 .. x `div` coins!!(n-1)]
  where addCoin k = map (++[k]) (withcoins (n-1) (x - k*coins!!(n-1)) )
 
problem_31 = length $ withcoins (length coins) 100
