module Ficha8 where


--Ex1
data Frac = F Integer Integer

--a
mdc :: Integer -> Integer -> Integer
mdc x y = maximum [n | n <- [1..(min x y)], mod x n == 0, mod y n == 0]

normaliza :: Frac -> Frac
normaliza (F x y) | x < 0 && y < 0 = (F (div (-x) (mdc (-x) (-y))) (div (-y) (mdc (-x) (-y)))) 
                  | x < 0 && y > 0 = (F (div (x) (mdc (-x) y)) (div (y) (mdc (-x) y)))
                  | x > 0 && y < 0 = (F (div (-x) (mdc x (-y))) (div (-y) (mdc x (-y))))
                  | otherwise = (F (div (x) (mdc x y)) (div (y) (mdc x y)))

--b
instance Eq Frac where
    (F x y) == (F a b) = x * b == y * a


--c
instance Ord Frac where
    (F x y) <= (F a b) = x * b <= y * a



--d
instance Show Frac where
    show (F x y) = show x ++ " / " ++ show y


--e
instance Num Frac where
    (F x y) + (F a b) = normaliza (F ((x*b)+(a*y)) (y*b))
    (F x y) - (F a b) = normaliza (F ((x*b)-(a*y)) (y*b))
    (F x y) * (F a b) = normaliza (F (x*a) (y*b))
    negate (F x y) = normaliza (F (-x) (y))
    abs (F x y) = normaliza (F (abs(x)) (abs (y)))
    signum (F x y) | x * y < 0 = -1
                   | x * y > 0 = 1
                   | x == 0 = 0
    fromInteger x = (F (x) (1))


compara :: Frac -> [Frac] -> [Frac]
compara (F x y) [] = []
compara (F x y) ((F a b):xs) | normaliza ((F x y) * (F 2 1))  < normaliza (F a b) = (F a b) : compara (F x y) xs
                             | otherwise = compara (F x y) xs



--Ex2 
data Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

 
calcula :: Num a => Exp a -> a
calcula (Const n) = n
calcula (Simetrico (Const n)) = (-n)
calcula (Mais (Const n) (Const m)) = n + m
calcula (Menos (Const n) (Const m)) = n - m
calcula (Mult (Const n) (Const m)) = n * m


--a
instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Simetrico a) = "- " ++ show a
  show (Mais a b) = show a ++ " + " ++ show b
  show (Menos a b) = show a ++ " - " ++ show b
  show (Mult a b) = show a ++ " * " ++ show b

--b
instance (Eq a,Num a) => Eq (Exp a) where
    (Const x) == (Const y) = x == y
    (Simetrico x) == (Simetrico y) = x == y
    (Mais x y) == (Const b) = calcula (Mais x y) == calcula (Const b)
    (Menos x y) == (Const b) = calcula (Menos x y) == calcula (Const b)
    (Mult x y) == (Const b) = calcula (Mult x y) == calcula (Const b)

--c
instance (Num a,Eq a) => Num (Exp a) where
    x + y = Const (calcula x + calcula y)
    x - y = Const (calcula x - calcula y)
    x * y = Const (calcula x * calcula y)
    negate (Const a) = Const (- a)
    negate (Simetrico a) = a
    negate (Mais a b) = Mais (- a) (- b)
    negate (Menos a b) = Menos b a
    negate (Mult a b) = Mult (-a) b
    fromInteger x = Const (fromInteger x)
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b)
    abs (Mult a b) = abs (a * b)
    signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
    signum (Simetrico a) = - signum a
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))

--Ex3