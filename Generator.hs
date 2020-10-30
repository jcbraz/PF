module Generator where

import System.Random

type Labirinto = [Corredor]
type Corredor = [Peca]
data Peca = Comida TamanhoComida | Parede | Chao
data TamanhoComida = Grande | Pequena

instance Show Peca where
    show (Comida Grande) = "o"
    show (Comida Pequena) = "."
    show (Parede) = "#"
    show (Chao) = " "

sampleMaze :: Labirinto
sampleMaze = [
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede],
                [Chao, Comida Grande, Comida Pequena, Comida Pequena, Comida Pequena, Comida Pequena, Comida Pequena, Chao],
                [Chao, Comida Pequena, Comida Pequena, Comida Pequena, Comida Pequena, Parede, Comida Pequena, Chao],
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede]
            ]


-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- Converssta list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converts an integer number into a Peca
-- 3 <=> Comida Granfre
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 99 <=> Parede
--
convertePeca :: Int -> Peca
convertePeca  x
    | x == 3 = Comida Grande
    | x >= 0 && x<70 = Comida Pequena
    | x >= 70 && x <= 99 = Parede


-- | Converts a Corredor to a string
--
printCorridor :: Corredor -> String
printCorridor [] = "\n"
printCorridor (h:t) = show h ++ printCorridor t

-- | Converts a Labirinto to a string
--
printMaze :: Labirinto -> String
printMaze [] = "" 
printMaze (h:t) = printCorridor h ++ printMaze t

-- | Converts a list of integers into a Corredor
--
converteCorredor :: [Int] -> Corredor
converteCorredor c = map (\p -> convertePeca p) c


-- | Converts a list of lists of integers into a Labirinto
--
converteLabirinto :: [[Int]] -> Labirinto
converteLabirinto l = map (\c -> converteCorredor c) l

----------------------------------------------------------------------------


geraLabirinto :: Int -> Int -> Int -> IO ()
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in putStrLn $ printMaze $ converteLabirinto $ subLista x random_nrs
                 

                 
criaparede :: Int -> Corredor
criaparede 0 = []
criaparede x = [Parede] ++ criaparede (x-1)


--casapar :: [[Peca]]
--casapar = [[Empty, Empty,...],
--bb