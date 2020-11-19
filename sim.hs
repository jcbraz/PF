module Exercicios where
import Data.List
import Data.Char

--Ex1
enumFromToc :: Int -> Int -> [Int]
enumFromToc x y | x == y = [x]
                | otherwise = x : enumFromToc (x+1) y

--Ex2
enumFromThenToc :: Int -> Int -> Int -> [Int]
enumFromThenToc x y z | z == x = [x]
                      | z < x =  []
                      | y < x = []
                      | otherwise = x : enumFromThenToc y (y+(y-x)) z

--Ex3
maismais :: [a] -> [a] -> [a]
maismais [] [] = []
maismais l [] = l
maismais [] l = l
maismais (x:xs) (y:ys) = x : maismais xs (y:ys)


--Ex4
excla :: [a] -> Int -> a
excla (h:t) x | x == 0 = h
              | otherwise = excla t (x-1)

--Ex5
reverte :: [a] -> [a]
reverte [] = []
reverte (h:t) = reverte t ++ [h]


--Ex6
tirar :: Int -> [a] -> [a]
tirar x [] = []
tirar x (h:t) | x > length (h:t) = (h:t)
              | x == 1 = [h]
              | otherwise = h : tirar (x-1) t


--Ex7
dropa :: Int -> [a] -> [a]
dropa x [] = []
dropa x (h:t) | x > length (h:t) = []
              | x == 1 = t
              | otherwise = dropa (x-1) t


--Ex8
zipa :: [a] -> [b] -> [(a,b)]
zipa [] [] = []
zipa l [] = []
zipa [] l = []
zipa (x:xs) (y:ys) = (x,y) : zipa xs ys


--Ex9
elemm :: Eq a => a -> [a] -> Bool
elemm x [] = False
elemm x (h:t) | x == h = True
              | otherwise = elemm x t

--Ex10
replica :: Int -> a -> [a]
replica 0 x = []
replica n x | n == 1 = [x]
            | otherwise = x : replica (n-1) x

--Ex11
inters :: a -> [a] -> [a]
inters x [] = [x]
inters x [a] = [a]
inters x (h:t) = h : x : inters x t


--Ex12
grupo :: Eq a => [a] -> [[a]]
grupo [] = []
grupo (h:t) = (h : aux t) : aux2 t
            where
                aux [] = []
                aux (x:xs) | x == h = h : aux xs
                           | otherwise = []
                aux2 [] = []           
                aux2 (y:ys)| y == h = aux2 ys
                           | otherwise = grupo (y:ys)
                


grupo2 :: Eq a => [a] -> [[a]]
grupo2 [] = []
grupo2 (h:t) = (h:aux t) : aux2 t
            where
                aux [] = []
                aux (x:xs) | x == h = h : aux xs
                           | otherwise = []
                aux2 [] = []          
                aux2 (y:ys)| y == h = aux2 ys
                           | otherwise = grupo2 (y:ys)

--Ex13
concato :: [[a]] -> [a]
concato [[]] = []
concato [] = []
concato (l : k : t) = l ++ k ++ concato t


--Ex14
initss :: [a] -> [[a]]
initss [] = [[]]
initss l = initss (init l) ++ [l]

--Ex15
taila :: [a] -> [[a]]
taila [] = [[]]
taila l = [l] ++ taila (init l)

--Ex16
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] [] = True
prefix [] l = True
prefix k [] = False
prefix (x:xs) (y:ys) | x == y && prefix xs ys = True
                     | otherwise = False


--Ex17
sufix :: Eq a => [a] -> [a] -> Bool
sufix [] [] = True
sufix l [] = True
sufix [] k = False
sufix l k | aux (reverse l) (reverse k) = True
          | otherwise = False
      where
          aux [] [] = True
          aux l [] = False
          aux [] l = True
          aux (x:xs) (y:ys) | x == y && aux xs ys = True
                            | otherwise = False

--Ex18
subse :: Eq a => [a] -> [a] -> Bool
subse [] [] = True
subse l [] = False
subse [] l = True
subse (x:xs) (y:ys) | x == y = subse xs ys
                    | otherwise = subse (x:xs) ys



--Ex19
elemin :: Eq a => a -> [a] -> [Int]
elemin x [] = []
elemin x (h:t) | x == h = 0 : map (+1) (elemin x t)
               | otherwise = map (+1) (elemin x t)


--Ex20
noob :: Eq a => [a] -> [a]
noob [] = []
noob (h:t) = h : (filter (/=h) (noob t))


--Ex21
delete2 :: Eq a => a -> [a] -> [a]
delete2 x [] = []
delete2 x (h:t) | x == h = t
                | otherwise = h : delete2 x t


--Ex22
barra :: Eq a => [a] -> [a] -> [a]
barra [] [] = []
barra l [] = l
barra [] l = []
barra (x:xs) (y:ys) | x == y = barra xs ys
                    | otherwise = x : barra xs (y:ys)


--Ex23
uniao :: Eq a => [a] -> [a] -> [a]
uniao [] [] = []
uniao l [] = l
uniao [] l = []
uniao l (x:xs) | elem x l == True = uniao l xs
               | otherwise = uniao (l ++ [x]) xs



--Ex24
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] [] = []
intersecao l [] = []
intersecao [] l = []
intersecao (x:xs) (y:ys) | x == y = x : intersecao xs (y:ys)
                         | otherwise = intersecao xs ys
--ou

intersecao2 :: Eq a => [a] -> [a] -> [a]
intersecao2 [] [] = []
intersecao2 l [] = []
intersecao2 [] l = []
intersecao2 l (x:xs) | elem x l = x : intersecao2 l xs
                     | otherwise = intersecao2 l xs

--Ex25
inserir :: Ord a => a ->[a] -> [a]
inserir x [] = [x]
inserir x (h:t) | x < h = x : (h:t)
                | otherwise = h : inserir x t


--Ex26
unpalavras :: [String] -> String
unpalavras [] = " "
unpalavras [x] = x
unpalavras (h:t) = h ++ " " ++ unpalavras (init (t)) ++ last t


--Ex27
unlinhas :: [String] -> String
unlinhas [] = ""
unlinhas [x] = x
unlinhas (h:t) = h ++ "\n" ++ unlinhas t


--Ex28
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) | h == aux (h:t) = 0
             | otherwise = 1 + pMaior t
    where
        aux [x] = x
        aux (x:y:ys) | x >= y = aux (x:ys)
                     | otherwise = aux (y:ys)
-------------------------------------------------
pMaior2 :: Ord a => [a] -> Int
pMaior2 [x] = 0
pMaior2 (h:t) | h == aux3 (h:t) = 0
              | otherwise = 1 + pMaior2 t
         where
              aux3 [x] = x
              aux3 (x:y:ys) | x >= y = aux3 (x:ys)
                            | otherwise = aux3 (y:ys)

--Ex29
temrepetidos :: Eq a => [a] -> Bool
temrepetidos [] = False
temrepetidos (h:t) | elem h t = False
                   | otherwise = temrepetidos t


--Ex30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | ord (h) >= 48 && ord (h) <= 57 = h : algarismos t
                 | otherwise = algarismos t


algarismos2 :: [Char] -> [Char]
algarismos2 [] = []
algarismos2 (h:t) | isDigit h = h : algarismos2 t
                  | otherwise = algarismos2 t




--Ex31
posimpares :: [a] -> [a]
posimpares [] = []
posimpares [x] = []
posimpares [x,y] = [y]
posimpares (x:y:z:k:t) = y : k : posimpares t



--Ex32
pospares :: [a] -> [a]
pospares [] = []
pospares [x] = [x]
pospares [x,y] = [x]
pospares (x:y:z:k:t) = x : z : pospares t


--Ex33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (a:c:t) | a <= c = isSorted t
                 | otherwise = False


--Ex34
iSort :: Ord a => [a] -> [a]            
iSort [] = []
iSort [x] = [x]
iSort (h:t) = insert h (iSort t)


--Ex35
menor :: String -> String -> Bool
menor "" "" = False
menor "" x = True
menor x "" = False
menor (x:xs) (y:ys) | ord x < ord y = True
                    | ord x == ord y = menor xs ys
                    | otherwise = False



--Ex36
elemmset :: Eq a => a -> [(a,Int)] -> Bool
elemmset x [] = False
elemmset x ((a,b):t) | x == a = True
                     | otherwise = elemmset x t


--Ex37
lengthmset :: [(a,Int)] -> Int
lengthmset [] = 0
lengthmset [x] = 1
lengthmset ((a,b):(c,d):t) = b + d + lengthmset t


--Ex38
convertemset :: [(a,Int)] -> [a]
convertemset [] = []
convertemset ((a,0):t) = convertemset t
convertemset ((a,b):t) = a : convertemset ((a,(b-1)):t)


convertemset2 :: [(a,Int)] -> [a]
convertemset2 [] = []
convertemset2 ((a,0):t) = convertemset2 t
convertemset2 ((a,b):t) = a : convertemset2 ((a,(b-1)):t)


--Ex39
inseremset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
inseremset a [] = [(a,1)]
inseremset x ((a,b):t) | x == a = (a,b+1):t
                       | otherwise = (a,b) : inseremset x t


--Ex40
removemset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removemset a [] = []
removemset x ((a,b):t) | x == a && b == 1 = t
                       | x == a && b > 1 = (a,b-1) : t
                       | otherwise = (a,b) : removemset x t
                   

--Ex41
constroiMSet :: Ord a  => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = (l,1 + length (filter (==l) ls)):constroiMSet (filter (/=l) ls)             






constroiMSet2 :: Ord a => [a] -> [(a,Int)]
constroiMSet2 [] = []
constroiMSet2 (h:t) = (h,1 + length (filter (==h) t)):constroiMSet' (filter (/=h) t)

--Ex42
partitioneitherss :: [Either a b] -> ([a],[b])
partitioneitherss l = (aux l,aux2 l)
               where
                   aux [] = []
                   aux ((Left a):ls) = a : aux ls
                   aux ((Right a):ls) = aux ls
                   aux2 [] = []
                   aux2 ((Left b):ls) = aux2 ls
                   aux2 ((Right b):ls) = b : aux2 ls


--Ex43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):t) = a : catMaybes t
catMaybes ((Nothing):a) = catMaybes t


data Movimento = Norte | Sul | Este | Oeste
               deriving Show


--Ex44
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (Norte:t) = (a,b+1)
posicao (a,b) (Sul:t) = (a,b+1)
posicao (a,b) (Este:t) = (a,b+1)
posicao (a,b) (Oeste:t) = (a-1,b)


--Ex45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho  (x,y) (x1,y1) | x == x1 && y == y1 = []
                       | x < x1 = [Este] ++ caminho (x+1,y) (x1,y1)
                       | x > x1 = [Oeste] ++ caminho (x-1,y) (x1,y1)
                       | y < y1 = [Norte] ++ caminho (x,y+1) (x1,y1)
                       | y > y1 = [Sul] ++ caminho (x,y-1) (x1,y1)


--Ex46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = aux4 h && vertical t
           where
               aux4 Norte = True
               aux4 Sul = True
               aux4 Este = False
               aux4 Oeste = False



data Posicao = Pos Int Int
             deriving Show

--Ex47
maiscentral :: [Posicao] -> Posicao
maiscentral [x] = x
maiscentral (x:y:t) | aux x <= aux y = maiscentral (x:t)
                    | otherwise = maiscentral (y:t)
                   where
                     aux (Pos g k) = sqrt(fromIntegral((g^2)+(k^2)))


--Ex48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) [] = []
vizinhos (Pos x y) ((Pos a b):t) | x == a && y == (b+1) = (Pos a b) : vizinhos (Pos x y) t
                                 | x == a && y == (b-1) = (Pos a b) : vizinhos (Pos x y) t
                                 | y == b && x == (a+1) = (Pos a b) : vizinhos (Pos x y) t
                                 | y == b && x == (a-1) = (Pos a b) : vizinhos (Pos x y) t
                                 | otherwise = vizinhos (Pos x y) t


--Ex49
mesmaordenada :: [Posicao] -> Bool
mesmaordenada [] = True
mesmaordenada [Pos x y] = True
mesmaordenada ((Pos x y):(Pos a b):t) | y == b = mesmaordenada ((Pos a b):t)
                                      | otherwise = False


data Semaforo = Verde | Amarelo | Vermelho
                deriving Show
    
--Ex50
--interseccaoOk :: [Semaforo] -> Bool
--interseccaoOk [] = True
--interseccaoOk ((h:k:t) = (aux h k) && interseccaoOk t
--                    where
--                        aux [] = True
--                        |aux Verde Vermelho = True
--                        |aux Vermelho Verde = True
--                        |aux Vermelho Vermelho = True
--                        |aux Vermelho Amarelo = True
--                        |aux Amarelo Vermelho = True
--                        |otherwise = aux t

--ou

interseccaoOk :: [Semaforo] -> Bool
interseccaoOk [] = True
interseccaoOk (Vermelho:t) = interseccaoOk t
interseccaoOk (h:t) = aux t
                   where
                       aux [] = True
                       aux (Vermelho:t) = aux t
                       aux _ = False








isSuffixOff :: Eq a => [a] -> [a] -> Bool
isSuffixOff [] l = True
isSuffixOff l [] = False
isSuffixOff (h:hs) (x:xs) = if last (h:hs) == last (x:xs) then isSuffixOff (init (h:hs)) (init (x:xs)) else False


constroiMSet' :: Ord a  => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (l:ls) = (l,1 + length (filter (==l) ls)):constroiMSet' (filter (/=l) ls)

