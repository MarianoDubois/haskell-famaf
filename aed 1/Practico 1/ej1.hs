--Lab1

esCero :: Int -> Bool
esCero x | x==0 = True
         | otherwise = False

esPositivo :: Int -> Bool
esPositivo x | x>0 = True
             | otherwise = False

esVocal :: Char -> Bool --If else case example
esVocal x | x=='a' || x=='e' || x=='i' || x=='o' || x=='u' = True
          | otherwise = False

esVocal2 :: Char -> Bool --Pattern Matching example
esVocal2 'a' = True
esVocal2 'e' = True
esVocal2 'i' = True
esVocal2 'o' = True
esVocal2 'u' = True
esVocal2 x = False

valorAbsoluto :: Int -> Int
valorAbsoluto x | x>0 = x
                | otherwise = -x 

--Lab2

esbooleano :: [Bool] -> Bool
esbooleano [] = True
esbooleano (x:xs) = x && esbooleano xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)

--Lab3

esPositivoList :: [Int] -> Bool
esPositivoList [] = True
esPositivoList (x:xs) = x>0 && esPositivoList xs

esIgual :: [Int] -> Int -> Bool
esIgual [] _ = False
esIgual (i:xs) x = (i == x) || esIgual xs x


todoIgual :: [Int] -> Bool
todoIgual [] = True
todoIgual [_] = True
todoIgual (x:y:xs) = (x == y) && todoIgual (y:xs)

todoIgual2 :: [Int] -> Int --Pattern Matching????
todoIgual [] = True
todoIgual (x:[]) = True
todoIgual (x:(y:ys)) = x==y && todoIgual (y:ys)

--Lab4

productoriaHastaN :: Int -> Int
productoriaHastaN x = factorial x

sumatoriaHastaN :: Int -> Int
sumatoriaHastaN 0 = 0
sumatoriaHastaN x = x + sumatoriaHastaN(x-1)

sumatoriaPromediada :: [Int] -> Int
sumatoriaPromediada [] = 0
sumatoriaPromediada xs = div (sumatoria xs) (length xs)

maxVSmin :: [Int] -> [Int] -> Bool
maxVSmin xs ys | maximum xs < minimum ys = True
               | otherwise = False

multiplicacionBinaria :: Int -> [Int] --REVEER
multiplicacionBinaria = undefined

--Lab5

todosTrue :: [Bool] -> Bool
todosTrue [] = True
todosTrue (x:xs) = x == True && todosTrue xs

--Lab6 

maxList :: [b] -> b   --REVEER
maxList [] = undefined

productoParesList :: [Int] -> Int
productoParesList [] = 1
productoParesList (x:xs) | x `mod` 2 == 0 = x * productoParesList xs
                         | otherwise = productoParesList xs

sumElementosPosicionPar :: [Int] -> Int
sumElementosPosicionPar [] = 0
sumElementosPosicionPar xs i = ((length xs)- i) `mod` 2 == 0 = (xs!i) + sumElementosPosicionPar xs