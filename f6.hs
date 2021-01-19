module Ficha6 where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
                deriving Show


--Exercicio1
--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node a l r) = 1 + max (altura l) (altura r)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a l r) = 1 + contaNodos l + contaNodos r

--c 
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a l r) = folhas l + folhas r


--d
prune :: Int -> BTree a -> BTree a
prune 0 (Node a l r) = Empty
prune n Empty = Empty
prune n (Node a Empty Empty) = (Node a Empty Empty)
prune n (Node a l r) = Node a (prune (n-1) l) (prune (n-1) r)


 
--e
path :: [Bool] -> BTree a -> [a]
path [] (Node a l r) = [a]
path l Empty = []  
path (x:xs) (Node a l r) | x == True = a : path xs r
                         | otherwise = a : path xs l

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a l r) = Node a (mirror r) (mirror l)


--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT p Empty Empty = Empty
zipWithBT p (Node t l r) Empty = Empty
zipWithBT p Empty (Node a e d) = Empty
zipWithBT p (Node t l r) (Node f e d) = Node (p t f) (zipWithBT p l e) (zipWithBT p r d)

--h
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) l r) = (aux (Node (a,b,c) l r), aux2 (Node (a,b,c) l r), aux3 (Node (a,b,c) l r))
                          where
                             aux :: BTree (a,b,c) -> BTree a
                             aux Empty = Empty
                             aux (Node (a,b,c) l r) = Node a (aux l) (aux r)
                             aux2 :: BTree (a,b,c) -> BTree b
                             aux2 Empty = Empty
                             aux2 (Node (a,b,c) l r) = Node b (aux2 l) (aux2 r)
                             aux3 :: BTree (a,b,c) -> BTree c
                             aux3 Empty = Empty
                             aux3 (Node (a,b,c) l r) = Node c (aux3 l) (aux3 r)



--Exercicio2
--a
minimo :: Ord a => BTree a -> a
minimo (Node a Empty r) = a
minimo (Node a l r) = minimo l

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty r) = Empty
semMinimo (Node a l r) = Node a (semMinimo l) r

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin node = (minimo node,semMinimo node)

--d
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node a l r) | x == a = Empty
                      | x < a = (Node a (remove x l) r)
                      | x > a = (Node a l (remove x r))


--Exercicio3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int 
                   | Rep
                   | Faltou
     deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


--a
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False 
inscNum n (Node (n2,no,r2,c) l r) | n == n2 = True 
                                  | otherwise = inscNum n l || inscNum n r

--b
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome no (Node (n2,no2,r2,c) l r) | no == no2 = True 
                                     | otherwise = inscNome no l || inscNome no r
  

--c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n2,no2,TE,c) e d) = [(n2,no2)] ++ trabEst e ++ trabEst d
trabEst (Node (n2,no2,r,c) e d) = trabEst e ++ trabEst d


--d
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota 0 turma = Nothing
nota n (Node (n2,no2,r,c) e d) | inscNum n (Node (n2,no2,r,c) e d) == False = Nothing
nota n (Node (n2,no2,r,c) e d) | n == n2 = Just c
                               | n < n2 = nota n d
                               | n > n2 = nota n e


--e
--percFaltas :: Turma -> Float
--percFaltas Empty = 100
--percFaltas (Node (n2,no2,r,c) e d) = (div (quemfaltou (Node (n2,no2,r,c) e d)) (total(Node (n2,no2,r,c) e d)))*100
--                                   where
--                                      quemfaltou :: Turma -> Float
--                                      quemfaltou Empty = 100
--                                      quemfaltou (Node (n2,no2,r,c) e d) = (case c of Faltou -> 1; otherwise -> 0 + quemfaltou e + quemfaltou d)
--                                      total :: Turma -> Float 
--                                      total Empty = 0
--                                      total (Node (n2,no2,r,c) e d) = 1 + total e + total d

percFaltas :: Turma -> Float
percFaltas Empty = 100
percFaltas (Node(n2,no2,r,c) e d) = (quemfaltou(Node (n2,no2,r,c) e d) / (total(Node (n2,no2,r,c) e d)))
                                where
                                  quemfaltou :: Turma -> Float
                                  quemfaltou Empty = 100
                                  quemfaltou (Node (n2,no2,r,Faltou) e d) = 1
                                  quemfaltou (Node (n2,no2,r,c) e d) = 0 + quemfaltou e + quemfaltou d
                                  total :: Turma -> Float 
                                  total Empty = 0
                                  total (Node (n2,no2,r,c) e d) = 1 + total e + total d




--f
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov (Node (n2,no2,r,c) e d) = (quempassou (Node (n2,no2,r,c) e d) / (totalv2(Node (n2,no2,r,c) e d)))
                                where
                                      quempassou :: Turma -> Float
                                      quempassou Empty = 0
                                      quempassou (Node (n2,no2,r,Aprov n) e d) = fromIntegral n 
                                      quempassou (Node (n2,no2,r,c) e d) = quempassou e + quempassou d
                                      totalv2 :: Turma -> Float 
                                      totalv2 Empty = 0
                                      totalv2 (Node (n2,no2,r,c) e d) = 1 + totalv2 e + totalv2 d


--g
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv x = y / w
          where
             (y,w) = aux x
                   where
                      aux :: Turma -> (Float,Float)
                      aux Empty = (0,0)
                      aux (Node (a,b,c,d) l r) = (case d of Aprov n -> (q+1,s);Rep -> (q,s+1); otherwise -> (q,s))
                                              where
                                                 (q,s) = (k+i,z+v)
                                                 (k,z) = (aux l)
                                                 (i,v) = (aux r)
                                                 
                                                 