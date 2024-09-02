--Lab1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use map" #-}

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

todoIgual2 :: [Int] -> Bool --Pattern Matching????
todoIgual2 [] = True
todoIgual2 (x:[]) = True
todoIgual2 (x:(y:ys)) = x==y && todoIgual2 (y:ys)

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
sumElementosPosicionPar [x] = x
sumElementosPosicionPar (x:y:xs) = x + sumElementosPosicionPar xs

--Lab 7

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) f = f x && paratodo' xs f

existe' :: [a] -> (a -> Bool) -> Bool 
existe' [] _ = False
existe' (x:xs) f = f x || existe' xs f

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) f = f x * productoria' xs f

--Lab 8

--Lab 9

todosPares' :: [Int] -> Bool
todosPares' xs = paratodo' xs even

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo x xs = existe' xs dividePorX  
  where dividePorX :: Int -> Bool
        dividePorX y = mod y x == 0 

sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] (^2)

existeDivisor :: Int -> [Int] -> Bool 
existeDivisor x xs = existe' xs dividePorY  
  where dividePorY :: Int -> Bool
        dividePorY y = mod x y == 0

esPrimo :: Int -> Bool
esPrimo x = not (existeDivisor x [2..x-1])

factorial' :: Int -> Int
factorial' x = productoria' [1..x] (*1)

{-
multiplicarPrimo :: [Int] -> Int --REVEER
multiplicarPrimo xs = productoria' (devuelvePrimo xs) id
  where devuelvePrimo :: [Int] -> [Int]
        devuelvePrimo x = (esPrimo x == True) = x 

--Tute solution

multiplicarPrimos :: [Int] -> Int
multiplicarPrimos xs = productoria' xs valorSiEsPrimo
  where valorSiEsPrimo :: Int -> Int
        valorSiEsPrimo x = if esPrimo x then x else 1

-}

multiplicarPrimo :: [Int] -> Int
multiplicarPrimo xs = productoria' xs devuelvePrimo
  where devuelvePrimo :: Int -> Int
        devuelvePrimo x | esPrimo x = x
                        | otherwise = 1

esFib :: Int -> Bool
esFib n = existe' [0..n+1] (\x -> fib x==n) 
  where fib :: Int -> Int
        fib 0 = 0
        fib 1 = 1
        fib x = fib (x-1) + fib (x-2)

todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib

-- 10

doubleListValues :: [Int] -> [Int]
doubleListValues [] = []
doubleListValues (x:xs) = 2*x : doubleListValues xs

doubleListValuesMap :: [Int] -> [Int]
doubleListValuesMap xs = map (2*) xs

-- 11

soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos (x:xs) | esPrimo x = x: soloPrimos xs
                  | otherwise = soloPrimos xs

soloPrimosFilter :: [Int] -> [Int]
soloPrimosFilter xs = filter esPrimo xs

multiplicarPrimoFilter :: [Int] -> Int
multiplicarPrimoFilter xs = productoria' (soloPrimosFilter xs) id 

-- 12
{-
primIgualesA :: a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA y (x:xs) = if y==x && x==xs!1 then primIgualesA y xs
                        if y==x && 
-}

primIgualesA :: Eq a => a -> [a] -> [a] --pq funca???
primIgualesA _ [] = []
primIgualesA y (x:xs) | y == x    = x : primIgualesA y xs
                      | otherwise = []

primIgualesATakeWhile :: Eq a => a -> [a] -> [a] --pq funca???
primIgualesATakeWhile _ [] = []
primIgualesATakeWhile y (x:xs) | y == x    = x : primIgualesA y xs
                               | otherwise = []

