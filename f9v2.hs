module Ficha9 where
import System.Random
import Data.List


--a

bingo :: IO()
bingo = do
	    aux [1..90]


aux :: (Show a,Eq a) => [a] -> IO()
aux [] = return ()
aux l = do
        n <- randomRIO (0,(length l)-1)
        getChar
        putStrLn (show (l !! n))
        aux (delete (l !! n) l)



---------------------------------------------

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover (x,y) img) = conta img
conta (Juntar (x:xs)) = conta x + conta (Juntar xs)



--b

apagaR :: Int -> Imagem -> Imagem
apagaR n (Mover coord imagem) = (Mover coord (apagaR n imagem))
apagaR n (Juntar x) = (Juntar (auxAP n (x)))

auxAP :: Int -> [Imagem] -> [Imagem]
auxAP 1 (x:xs) = xs
auxAP n (x:xs) = x : auxAP (n-1) xs


apagaN :: Imagem -> IO Imagem
apagaN x = do
	       n <- randomRIO (1,conta x)
	       return (apagaR n x)

