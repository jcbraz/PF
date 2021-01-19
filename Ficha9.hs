module Ficha9 where

import System.Random
import Data.List
import Data.Char
import qualified Data.Text as Text

-- | 1)

-- a)

bingo :: IO ()
bingo = do let l = [1..90]
           bingoaux l

bingoaux :: (Eq a,Show a) => [a] -> IO()
bingoaux [] = return ()
bingoaux l  = do i <- randomRIO (0, length l-1)
                 getChar
                 putStrLn (show (l!!i))
                 bingoaux (delete (l!!i) l)

-- b)

mastermind :: IO ()
mastermind = do (a,b,c,d) <- codigoN
                tentativaN (a,b,c,d)
                return ()

codigoN :: IO (Int,Int,Int,Int)
codigoN = do a <- randomRIO (0,9)
             b <- randomRIO (0,9)
             c <- randomRIO (0,9)
             d <- randomRIO (0,9)
             return (a,b,c,d)

analisarTentaiva :: IO (Int,Int,Int,Int)
analisarTentaiva = do x <- getLine
                      return (let (a:b:c:d:xs) = x in (read [a],read [b],read [c],read [d]))

tentativaN :: (Int,Int,Int,Int) -> IO ()
tentativaN (a,b,c,d) = do
            let combinacao = [a,b,c,d]
            (x,y,w,z) <- analisarTentaiva
            let corretos = 0 + (if a == x then 1 else 0) + (if b == y then 1 else 0) + (if c == w then 1 else 0) + (if d == z then 1 else 0)
            let errados = 0 + (if a /= x && (a == y || a == w || a == z) then 1 else 0) + (if b /= y && (b == x || b == w || b == z) then 1 else 0) + (if c /= w && (c == x || c == y || c == z) then 1 else 0) + (if d /= z && (d == y || d == w || d == x) then 1 else 0)
            if corretos == 4 then print "Parabens!! Ganhaste!" else print ("Numero de Valores Corretos: " ++ show corretos ++ "      " ++ "Numero de Valores no Sitio Errado: " ++ show errados )
            if corretos == 4 then return () else tentativaN (a,b,c,d)

-- | 2)

data Aposta = Ap [Int] (Int,Int)

-- a)

checkNum :: [Int] -> Bool
checkNum [] = True
checkNum (x:xs) = if x >= 1 && x <= 50 then checkNum xs else False

checkStar :: (Int,Int) -> Bool
checkStar (a,b) | a /= b && a >= 1 && a <= 9 && b >= 1 && b <= 9 = True
                | otherwise = False

valida :: Aposta -> Bool
valida (Ap x s) | length x == length (nub x) && length x == 5 && checkNum x == True && checkStar s == True = True
                | otherwise = False

-- b)

compareNum :: [Int] -> [Int] -> Int
compareNum [] y = 0
compareNum (x:xs) y = if x `elem` y then 1 + compareNum xs y else compareNum xs y

compareStar :: (Int,Int) -> (Int,Int) -> Int
compareStar (x,y) (a,b) | (x == a || x == b) && (y /= a && y /= b) = 1
                        | (x /= a && x /= b) && (y == a || y == b) = 1
                        | (x == a || x == b) && (y == a || y == b) = 2
                        | otherwise = 0

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap x s) (Ap c e) = (compareNum x c, compareStar s e)

-- c)

-- || -- i)

instance Eq Aposta where
    (Ap x s) == (Ap c e) = comuns (Ap x s) (Ap c e) == (5,2)

-- || -- ii)

premio :: Aposta -> Aposta -> Maybe Int
premio x y | comuns x y == (5,2) = Just 1
           | comuns x y == (5,1) = Just 2
           | comuns x y == (5,0) = Just 3
           | comuns x y == (4,2) = Just 4
           | comuns x y == (4,1) = Just 5
           | comuns x y == (4,0) = Just 6
           | comuns x y == (3,2) = Just 7
           | comuns x y == (2,2) = Just 8
           | comuns x y == (3,1) = Just 9
           | comuns x y == (3,0) = Just 10
           | comuns x y == (1,2) = Just 11
           | comuns x y == (2,1) = Just 12
           | comuns x y == (2,0) = Just 13
           | otherwise = Nothing

-- d)

-- || -- i)

unspace :: String -> [String]
unspace str = (map (Text.unpack) (Text.split (==' ') (Text.pack str)))

leAposta :: IO Aposta
leAposta = do 
           print "Introduza os Numeros (Separados por um Espaco)"
           x <- getLine
           print "Introduza as Estrelas (Separadas por um Espaco)"
           y <- getLine
           let aposta = (Ap (map (read) (unspace x)) (let (a:b:xs) = (unspace y) in (read a, read b)))
           if valida aposta == True then return aposta else do print "Aposta Invalida"
                                                               leAposta

-- || -- ii)

removeMaybe :: Maybe Int -> Int
removeMaybe Nothing = 0
removeMaybe (Just n) = n

joga :: Aposta -> IO ()
joga x = do
         y <- leAposta
         print ("Premio: " ++ show (removeMaybe (premio x y)))

-- e)

geraChave :: IO Aposta
geraChave = do
    nums <- generate 'N' []
    [star1,star2] <- generate 'S' []
    return (Ap nums (star1,star2))

generate :: Char -> [Int] -> IO [Int]
generate c l = do
    n <- randomRIO (1,(if c == 'N' then 50 else 9))
    if length l == 5 && c == 'N' then return l 
    else if length l == 2 && c == 'S' then return l 
    else if n `elem` l then generate c l else generate c (n:l)

main :: IO ()
main = do ch <- geraChave
          ciclo ch

ciclo :: Aposta -> IO ()
ciclo ch = do
    menuOpt <- menu
    case menuOpt of "1" -> do joga ch; ciclo ch
                    "2" -> do putStrLn "Nova chave gerada"; main
                    "0" -> return ()


menu :: IO String
menu = do putStrLn menutxt
          putStr "Opcao: "
          c <- getLine
          return c
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]

           

