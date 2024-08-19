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

par :: Int -> Bool
par x = mod x 2 == 0

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | par x    = x : soloPares xs
                 | otherwise = soloPares xs 

casiCuadrado :: Int -> Int
casiCuadrado x = 2*x^2+4

maximoDistinto :: Int -> Int -> Int
maximoDistinto x y | x <= y = y
                   | x > y = x

{-
disc :: Num -> Num -> Num -> Num
disc a b c = b^2-4*a*c

cuadratica :: Num -> Num -> Num -> Num
cuadratica a b c = -b+(sqrt(disc a b c))/2*a
-}

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

sumarTodo :: [Int] -> Int
sumarTodo [] = 0
sumarTodo(x:xs) =  x + sumarTodo xs

sumarTodoLista :: [Int] -> [Int]
sumarTodoLista [] = [0]
sumarTodoLista(x:xs) = [x + sum | sum <- sumarTodoLista xs]

entre0y9 :: Int -> Bool
entre0y9 x | x >= 0 && x <= 9 = True
           | otherwise = False