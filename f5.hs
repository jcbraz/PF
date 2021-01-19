module Ficha5 where
import Data.List


--Ex1

--a
any2 :: (a -> Bool) -> [a] -> Bool
any2 p [] = False
any2 p  (x:xs) = if (p x) then True
                 else any2 p xs


--ou

any3 :: (a -> Bool) -> [a] -> Bool
any3 p [] = False
any3 p (x:xs) | (p x) = True
              | otherwise = any2 p xs

--b
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 p (x:xs) (y:ys) = p x y : zipWith2 p xs ys
zipWith2 _ _ _ = []

--ou

zipWithv2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithv2 p [] [] = []
zipWithv2 p l [] = []
zipWithv2 p [] l = []
zipWithv2 p (x:xs) (y:ys) = p x y : zipWith2 p xs ys

--c
takeWhilev2 :: (a -> Bool) -> [a] -> [a]
takeWhilev2 p [] = []
takeWhilev2 p (x:xs) | p x = x : takeWhilev2 p xs
                     | otherwise = []

--d
dropWhilev2 :: (a -> Bool) -> [a] -> [a]
dropWhilev2 p [] = []
dropWhilev2 p (x:xs) | p x = dropWhilev2 p xs
                     | otherwise = x : dropWhilev2 p xs

--e
spanv2 :: (a -> Bool ) -> [a] -> ([a],[a])
spanv2 p [] = ([],[])
spanv2 p (x:xs) = (takeWhilev2 p (x:xs), dropWhilev2 p (x:xs))


--f
deleteByv2 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteByv2 p e [] = []
deleteByv2 p e (x:xs) | p e x = xs
                      | otherwise  = e : deleteByv2 p e xs 


--g (duvida)
sortOnv2 :: Ord b => (a -> b) -> [a] -> [a]
sortOnv2 p [] = []
sortOnv2 p (x:xs) = aux x (sortOnv2 p xs)
                  where
                      aux e [] = [e]
                      aux e (y:ys) | p e > p y = y : aux e ys
                                   | otherwise = e : y : ys
                    

--Ex2
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--fold,map,filter,foldl,sum e exercicio 1
--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(x,y) -> y == n) p



--selgrau :: Int -> Polinomio -> Polinomio
--selgrau x [] = []
--selgrau x ((a,b):s) | x == b = (a,b) : selgrau x s
--                    | otherwise = selgrau x s 


--b
conta :: Int -> Polinomio -> Int
conta n p = length (filter (\(x,y) -> y == n) p)





--conta :: Int -> Polinomio -> Int
--conta n [] = 0
--conta n ((a,b):xs) | n == b = 1 + conta n xs
--                   | otherwise = conta n xs


--c
grau :: Polinomio -> Int
grau p = snd (last (sortOn snd p))




--grau :: Polinomio -> Int
--grau [] = 0
--grau [(a,x),(b,y)]  | x < y = y
--                    | otherwise = x
--grau (c:c2:cs) | c2 < c = grau (c:cs)
--               | otherwise = grau (c2:cs)


--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(x,y) -> (x*(fromIntegral y),y-1)) (filter (\(x,y) -> y /= 0) p)




--deriv :: Polinomio -> Polinomio
--deriv [] = []
--deriv ((a,b):cs) | b == 0 = deriv cs
--                 | b == 1 = (a,0) : deriv cs
--                 | otherwise = (a*fromIntegral b,b-1) : deriv cs


--e
calcula :: Float -> Polinomio -> Float
calcula r p = foldl (\n (x,y) -> (n+(x*(r^y)))) 0 p 



--calcula :: Float -> Polinomio -> Float
--calcula n [] = 0
--calcula n ((a,b):xs) = a*(n^b) + calcula n xs


--f
simp :: Polinomio -> Polinomio
simp p = filter (\(x,y) -> y /= 0) p


--simp :: Polinomio -> Polinomio
--simp [] = []
--simp ((a,b):xs) | b == 0 = simp xs
--                | otherwise = (a,b) : simp xs


--g
mult :: Monomio -> Polinomio -> Polinomio
mult (m,m2) p = map (\(a,b) -> (a*m,(b+m2))) p 


--mult :: Monomio -> Polinomio -> Polinomio
--mult m [] = []
--mult (a,b) ((c,d):xs) | b == 0 = (c,d):xs
--                      | otherwise = (a*c,b+d) : mult (a,b) xs


--h
ordena :: Polinomio -> Polinomio
ordena p = sortOn snd p



--ordena :: Polinomio -> Polinomio
--ordena [] = []
--ordena [(a,b),(c,d)] | grau [(a,b)] > grau [(c,d)] = [(c,d),(a,b)]
--ordena ((a,b):(c,d):xs) | grau [(a,b)] > grau [(c,d)] = (c,d) : ordena ((a,b):xs)
--                        | otherwise = ordena ((c,d):xs)


--i(criar auxiliar)
normaliza :: Polinomio -> Polinomio
normaliza x = aux (ordena x)
     where      
         aux :: Polinomio -> Polinomio
         aux [] = []
         aux [(a,b)] = [(a,b)] 
         aux ((a,b):(c,d):t) | b == d = aux ((a+c,b):t)
                             | otherwise = (a,b) : aux ((c,d):t)




--normaliza :: Polinomio -> Polinomio
--normaliza [] = []
--normaliza [(a,b),(c,d)] | b == d = [(a+c,b)]
--                        | otherwise = [(a,b),(c,d)]
--normaliza ((a,b):(c,d):xs) | b == d = (a+c,b) : normaliza xs
--                           | otherwise = normaliza ((a,b):xs)


--j
soma :: Polinomio -> Polinomio -> Polinomio
soma x p = normaliza (x ++ p) 

--soma :: Polinomio -> Polinomio -> Polinomio
--soma [] [] = []
--soma ((a,b):t) ((c,d):t2) | b == d = (a+c,b) : soma t t2
--                          | otherwise = soma ((a,b):t) t2

--k 
produto :: Polinomio -> Polinomio -> Polinomio
produto [] [] = []
produto t [] = []
produto [] t = []
produto ((a,b):t) p = mult (a,b) p ++ produto t p



--produto :: Polinomio -> Polinomio -> Polinomio
--produto [] [] = []
--produto ((a,b):xs) p = mult (a,b) p ++ produto xs p 


--l
equiv :: Polinomio -> Polinomio -> Bool 
equiv x p | normaliza x == normaliza p = True 
          | otherwise = False 




--equiv :: Polinomio -> Polinomio -> Bool
--equiv [] [] = True
--equiv p p2 | normaliza p == normaliza p2 = True
--           | otherwise = False



--Ex3
type Mat a = [[a]]

--a
dimOK :: Mat a -> Bool
dimOK [[]] = True
dimOK [a] = True 
dimOK (a:b:t) | length a == length b && dimOK (b:t) = True
              | otherwise = False

--b
dimMat :: Mat a -> (Int,Int)
dimMat (a:b:t) = (length a,length (a:b:t))


--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat l [] = []
addMat (a:t) (c:t2) = aux a c : addMat t t2
                              where
                                  aux :: Num a => [a] -> [a] -> [a]
                                  aux [] [] = []
                                  aux l [] = l
                                  aux (x:xs) (y:ys) = x+y : aux xs ys



--d
transposev2 :: Mat a -> Mat a
transposev2 (l:t) =  manilists l t
    where
        giveheads :: Mat a -> [a]
        giveheads [] = []
        giveheads (x:xs) = head x : giveheads xs
        givesecond :: Mat a -> Mat a
        givesecond [] = []
        givesecond (x:xs) = tail x : givesecond xs
        manilists :: [a] -> Mat a -> Mat a
        manilists [] (y:ys) = []
        manilists (x:xs) (y:ys) = [x : giveheads (y:ys)] ++ manilists xs (givesecond (y:ys)) 


--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] [] = []
multMat [] (y:ys) = []
multMat (x:xs) (y:ys) = second x (y:ys) : multMat xs (y:ys)
    where
        first :: Num a => [a] -> [a] -> a
        first [] [] = 0
        first (a:as) [] = 0
        first (a:as) (b:bs) = (a*b) + first as bs
        second :: Num a => [a] -> Mat a -> [a]
        second [] [] = []
        second l [] = []
        second (c:cs) (d:ds) = first (c:cs) d : second (c:cs) ds
        

--f


--g
triSup :: (Eq a,Num a) => Mat a -> Bool
triSup x = matriz 1 x
    where
        linesm :: (Eq a,Num a) => Int -> [a] -> Bool
        linesm 1 (x:xs) = True
        linesm n (x:xs) | x == 0 = linesm (n-1) xs
                        | otherwise = False 
        matriz :: (Eq a,Num a) => Int -> Mat a -> Bool
        matriz n [] = True
        matriz n [a] = True
        matriz n (x:y:xs) = linesm n y && matriz (n+1) (y:xs)


--h
rotateLeft :: Mat a -> Mat a
rotateLeft x = reverse (transposev2 x)