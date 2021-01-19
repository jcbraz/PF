module F4 where
import Data.Char
import Data.List

--Ex1
--a) [6,12,18]
--b) [6,12,18]
--c) [(15,15),(14,16),(13,17),(12,18),(11,19),(10,20),(20,10),(19,11),(18,12),(17,13),(16,14)]
--d) []

--Ex2
alinea = [2^x | x <- [0..10]]
alineab = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]
alineac = [[1..x] | x <- [1..5]]
alinead = [replicate x 1 | x <- [1..5]]
-- 6! = 720
alineae = [factorial x | x <- [1..6]]
          where
              factorial 0 = 1
              factorial x = x * factorial (x-1)


--Ex3
digitAlpha :: String -> (String,String)
digitAlpha k = foldl (\(x,y) r -> if isDigit r then (x,y ++ [r]) else (x ++ [r],y)) ("","") k 


--Ex4
nzp :: [Int] -> (Int,Int,Int)
nzp k = foldl (\(x,y,z) r -> if r < 0 then (x+1,y,z) else if r == 0 then (x,y+1,z) else (x,y,z+1)) (0,0,0) k


nzpv2 :: [Int] -> (Int,Int,Int)
nzpv2 k = foldr (\r (x,y,z) -> if r < 0 then (x+1,y,z) else if r == 0 then (x,y+1,z) else (x,y,z+1)) (0,0,0) k


--Ex5
divModv2 :: Integral a => a -> a -> (a,a)
divModv2 x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y,y*2..x]

--Ex6
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsac l 0
     where
         fromDigitsac [] ac = ac
         fromDigitsac (h:t) ac = fromDigitsac t (h*10^(length t)+ac) 
--fromDigits [] = 0
--fromDigits (h:t) = h*10^(length t) + fromDigits t

--type Btree = 
 