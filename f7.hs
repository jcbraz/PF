module Ficha7 where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt



--Ex1
--a
calcula :: ExpInt -> Int
calcula (Const n) = n
calcula (Simetrico (Const n)) = -n
calcula (Mais (Const n) (Const m)) = n + m
calcula (Menos (Const n) (Const m)) = n - m
calcula (Mult (Const n) (Const m)) = n * m

 
--b(queda)
infixa :: ExpInt -> String
infixa (Const n) = (show n)
infixa (Simetrico n) = " - " ++ infixa n
infixa (Mais n m) = infixa n ++ " + " ++ infixa m 
infixa (Menos n m) = infixa n ++ " - " ++ infixa m
infixa (Mult n m) = infixa n ++ " * " ++ infixa m


--c
posfixa :: ExpInt -> String
posfixa (Const n) = (show n)
posfixa (Simetrico n) = posfixa n ++ " - "
posfixa (Mais n m) = posfixa n ++ " " ++  posfixa m ++ " + " 
posfixa (Menos n m) = posfixa n ++ " " ++ posfixa m ++ " - "
posfixa (Mult n m) = posfixa n ++ " " ++ posfixa m ++ " * "

--Ex2
data RTree a = R a [RTree a]

--a
soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a b) = a + sum (map soma b)



--b
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)


--c
prune :: Int -> RTree a -> RTree a
prune n (R a []) = R a []
prune n (R a l) = R a (map (prune (n-1)) l)

--d
mirror :: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a l) = R a (map mirror (reverse l))


--e
postorder :: RTree a -> [a]
postorder (R a []) = []
postorder (R a l) = concat (map postorder l) ++ [a]



--Ex3
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show 
data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

--a
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork l r) = ltSum l + ltSum r

--b
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork l r) = listaLT l ++ listaLT r


--c
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)


--Ex4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree x = (aux x,aux2 x)
       where
aux :: FTree a b -> BTree a
aux (Leaf b) = Empty
aux (No a l r) = (Node a (aux l) (aux r))
aux2 :: FTree a b -> LTree b
aux2 (Leaf b) = Tip b
aux2 (No a l r) = (Fork (aux2 l) (aux2 r))





--b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Fork e d) = Nothing
joinTrees (Node a l r) (Tip b) = Nothing 
joinTrees x y = Just (aux5 x y)
         where
aux5 :: BTree a -> LTree b -> FTree a b
aux5 (Empty) (Tip b) = Leaf b
aux5 (Node a l r) (Fork e d) = (No a (aux5 l e) (aux5 r d))



