module F3 where


--Ex1
data Hora = H Int Int
          deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


hora2 :: Hora -> Hora -> Bool
hora2 (H x y) (H x2 y2) | x < x2 && x2 < 24 = True
                        | x == x2 && y < y2 && y2 < 60 = True
                        | otherwise = False

converter :: (Int, Int) -> Int
converter (x1,y2) = (60*x1) + y2 

--converter3 :: (Hora,Hora) -> Int
--converter3 (H x1 y1,H x2 y2) | x < x2 && x2 < 24 && x2 >= 0 && x >= 0 && x < 24 && y2 < 60 && y2 >= 0 && y >= 0 && y < 60 = ((60*x2) + y2) + ((60*x1) + y1) 
--                             | x == x2 && x2 < 24 && x2 >= 0 && x >= 0 && x < 24 && y < y2 && y2 < 60 && y2 >= 0 && y >= 0 && y < 60 = ((60*x2) + y2) + ((60*x1) + y1) 
--a)
verifytime :: Etapa -> Bool
verifytime (H x y,H x2 y2) | x < x2 && x2 < 24 && x2 >= 0 && x >= 0 && x < 24 && y2 < 60 && y2 >= 0 && y >= 0 && y < 60 = True
                           | x == x2 && x2 < 24 && x2 >= 0 && x >= 0 && x < 24 && y < y2 && y2 < 60 && y2 >= 0 && y >= 0 && y < 60 = True
                           | otherwise = False


--b)
verifytrip :: Viagem -> Bool
verifytrip [] = True 
verifytrip [(h1,h2)] | verifytime (h1,h2) = True
                     | otherwise = False 
verifytrip ((h1,h2):(h3,h4):t) | verifytime (h1,h2) && verifytime (h3,h4) && verifytrip ((h3,h4):t) = True 
                               | otherwise = False


--c)
tripcalculator :: Viagem -> Etapa
tripcalculator [(h1,h2)] = (h1,h2)
tripcalculator [(h1,h2),(h3,h4)] = (h1,h4)
tripcalculator ((h1,h2):(h3,h4):t) = tripcalculator ((h1,h2):t)


--d) (Falta recursiva)

--clockconverter :: Hora -> Int
--clockconverter (H x y) = (x*60) + y


--clockconverterv2 :: Int -> Hora
--clockconverterv2 z = H (fromInteger(div z 60)) ((z/60) - fromInteger(div z 60))


timecalculator :: Etapa -> Hora
timecalculator (H x y,H x2 y2) | y <= y2 = H (x2-x) (y2-y)
                               | otherwise = H (x2-x-1) (60-y+y2)



--e) (Falta recursiva)
waittime :: Viagem -> Hora
waittime [(H x y, H x2 y2)] = H 00 00
waittime ((H x y, H x2 y2):(H x3 y3, H x4 y4):t) | y2 <= y3 = H (x3-x2) (y3-y2)
                                                 | otherwise = H (x3-x2-1) (60-y2+y3)


--f)
--totaltrip :: Viagem -> Hora
--totaltrip [(H x y, H x2 y2)] = 



--Exercicio3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]

--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n,[Email e])]
acrescEmail n e [(n2,l)] | n == n2 = [(n, l ++ [Email e])]  
                         | otherwise = [(n2,l),(n,[Email e])]
acrescEmail n e ((n2,l):xs) | n == n2 = [(n, l ++ [Email e])] ++ xs
                            | otherwise = acrescEmail n e xs


--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n [(n2,l)]      | n == n2 = Just (justx (auxemail l))
verEmails n ((n2,l):xs)   | n == n2 = Just (justx (auxemail l))
                          | otherwise = verEmails n xs
                     

auxemail :: [Contacto] -> [Contacto]
auxemail [] = []
auxemail ((Casa x):xs) = auxemail xs
auxemail ((Trab x):xs) = auxemail xs
auxemail ((Tlm x):xs) = auxemail xs
auxemail ((Email x):xs) = (Email x) : auxemail xs 

justx :: [Contacto] -> [String]
justx [] = []
justx [Email x] = [x]
justx ((Email x):xs) = x : justx xs


--c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa x):xs) = x : consTelefs xs
consTelefs ((Trab x):xs) = x : consTelefs xs
consTelefs ((Tlm x):xs) = x : consTelefs xs
consTelefs ((Email x):xs) = consTelefs xs 


--d)
casa :: Nome -> Agenda -> Maybe [Integer ]
casa n [] = Nothing
casa n [(n2,l)]      | n == n2 = Just (justhome (auxetlm l))
casa n ((n2,l):xs)   | n == n2 = Just (justhome (auxetlm l))
                     | otherwise = casa n xs

auxetlm :: [Contacto] -> [Contacto]
auxetlm [] = []
auxetlm ((Casa x):xs) = (Casa x) : auxetlm xs
auxetlm ((Trab x):xs) = auxetlm xs
auxetlm ((Tlm x):xs) = auxetlm xs
auxetlm ((Email x):xs) = auxetlm xs 

justhome :: [Contacto] -> [Integer]
justhome [] = []
justhome [Casa x] = [x]
justhome ((Casa x):xs) = x : justhome xs



--Ex4

type Dia = Int
type Mes = Int
type Ano = Int


data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]


--a)
procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((n2,date):t) | n == n2 = Just date
                        | otherwise = procura n t


--b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade date n [] = Nothing
idade (D x y z) n ((n2,D x2 y2 z2):t) | n == n2 && y > y2 = Just (z-z2-1)
                                      | n == n2 && y2 > y = Just (z-z2)
                                      | otherwise = idade (D x y z) n t


--c)
anterior :: Data -> Data -> Bool
anterior (D x y z) (D x2 y2 z2) | z < z2 = True 
                                | z == z2 && y == y2 && x == x2 = False
                                | z == z2 && y == y2 && x < x2 = True
                                | z == z2 && y < y2 = True
                                | otherwise = False 


--d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena (x:xs) = pa (ordena xs) x


pa :: TabDN -> (Nome,Data) -> TabDN
pa [] (x,y) = [(x,y)]
pa ((n,date):t) (n2,date2) | anterior date2 date = (n2,date2):(n,date):t
                           | otherwise = (n,date) : pa t (n2,date2)

--e) (duvida)
porIdade :: Data -> TabDN -> [(Nome,Int )]
porIdade (D x y z) [] = []
porIdade (D a b c) (x:xs) = idadev2 (D a b c) (reverse (ordena (x:xs)))


idadev2 :: Data -> TabDN -> [(Nome,Int)]
idadev2 date [] = []
idadev2 (D x y z) ((n2,D x2 y2 z2):t) | y > y2 =  (n2,z-z2-1) : idadev2 (D x y z) t
                                      | y2 > y =  (n2,z-z2) : idadev2 (D x y z) t
                                      | y2 == y && x < x2 = (n2,z-z2) : idadev2 (D x y z) t
                                      | y2 == y && x > x2 = (n2,z-z2-1) : idadev2 (D x y z) t
                                      | otherwise = idadev2 (D x y z) t



--Ex5
data Movimento = Credito Float | Debito Float
               deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

--a)
extvalor :: Extracto -> Float -> [Movimento]
extvalor (Ext k []) _ = []
extvalor (Ext k ((date,str,mov):t)) x = case mov of Credito y -> if y >= x then mov : extvalor (Ext k t) x else extvalor (Ext k t) x
                                                    Debito y -> if y >= x then mov : extvalor (Ext k t) x else extvalor (Ext k t) x



--b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext k []) [] = []
filtro (Ext k []) l = []
filtro (Ext k ((date,str,mov):t)) (str2:ls) | elem str (str2:ls) = (date,mov) : (filtro (Ext k t) (str2:ls))
                                            | otherwise = (filtro (Ext k t) (str2:ls))

--ou 

filtrov2 :: Extracto -> [String] -> [(Data,Movimento)]
filtrov2 (Ext k []) [] = []
filtrov2 (Ext k []) l = []
filtrov2 (Ext k ((date,str,mov):t)) (str2:ls) | carctidentify str (str2:ls) == True = [(date,mov)]
                                            | otherwise = []


carctidentify :: String -> [String] -> Bool
carctidentify s (l:ls) | s == l && carctidentify s ls = True
                       | otherwise = False 


--c) 
creDeb :: Extracto -> (Float,Float)
creDeb (Ext k []) = (0.0,0.0)
creDeb (Ext k ((date,str,mov):t)) = case mov of Credito y -> (y+aux t,aux2 t)
                                                Debito y -> (aux t,y+aux2 t)
                                 where
                                     aux [] = 0
                                     aux ((a,b,Credito y):t) = y + aux t
                                     aux ((a,b,Debito y):t) = aux t
                                     aux2 [] = 0
                                     aux2 ((a,b,Credito y):t) = aux2 t
                                     aux2 ((a,b,Debito y):t) = y + aux2 t


--d)
saldo :: Extracto -> Float
saldo (Ext k []) = 0
saldo (Ext k ((date,str,mov):t)) = case mov of Credito y -> aux t + y + k
                                               Debito y -> aux t - y + k
                                where
                                    aux [] = 0
                                    aux ((a,b,Credito y):t) = y + aux t
                                    aux ((a,b,Debito y):t) = (-y) + aux t