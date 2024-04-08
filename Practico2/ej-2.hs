{-
a) soloPares :: [Int] -> [Int], que dada una lista de enteros xs devuelve una lista sólo con los
números pares contenidos en xs, en el mismo orden y con las mismas repeticiones (si las hubiera).
Por ejemplo: soloPares [3,0,-2,12] = [0,-2, 12]
b) mayoresQue10 :: [Int] -> [Int], que dada una lista de enteros xs devuelve una lista sólo con los
números mayores que 10 contenidos en xs,
Por ejemplo: mayoresQue10 [3,0,-2, 12] = [12]
2
c) mayoresQue :: Int-> [Int] -> [Int], que dado un entero n y una lista de enteros xs devuelve
una lista sólo con los números mayores que n contenidos en xs,
Por ejemplo: mayoresQue 2 [3,0,-2, 12] = [3,12]
-}
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | even x    = x : soloPares xs
                 | otherwise = soloPares xs 

maximo :: [Int] -> Int
maximo [] = error "empty list"
maximo [x] = x
maximo (x:xs) =  max x (maximo xs)

ultimo :: [Int] -> Int
ultimo [x] = x
ultimo (x:y:xs) =  ultimo(y:xs)

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) =  rev xs ++ [x]

apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((a,b,c):xs) = b : apellidos xs

encadenar :: [[a]] -> [a]
encadenar [] = []          
encadenar (x:xs) = x ++ encadenar xs

takear :: Int -> [a] -> [a]
takear 0 xs = []
takear n [] = []
takear n (x:xs) = x : takear(n-1) xs