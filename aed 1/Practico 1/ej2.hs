{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE BlockArguments #-}
import GHC.Base (TrName, VecElem (Int16ElemRep), absentErr)
import Prelude hiding (pi)
import System.Win32 (xBUTTON1)


-----------------------------------------------------------{ 2 }----------------------------------------------------------------------------
--a
data Carrera = Matematica | Fisica | Computacion | Astronomia

--b
titulo :: Carrera -> String
titulo Matematica = "Lic en matematica"
titulo Fisica = "Lic en fisica"
titulo Computacion = "Lic en computacion"
titulo Astronomia = "Lic en astronomia"

--c
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si --tipo de dato finito
  deriving Eq                                       --actividad numero 3 a
  deriving Ord
  deriving Bounded
  deriving Show

--d
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do  = 'C'
cifradoAmericano Re  = 'D'
cifradoAmericano Mi  = 'E'
cifradoAmericano Fa  = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La  = 'A'
cifradoAmericano Si  = 'B'

-----------------------------------------------[ clase del 2/09 ]---------------------------------------------------------------------------

{-
en el polimorfismo parametrico no importa para nada el tipo que sea con tal que cumpla la condicion de tipos por asi decirlo
en el polimorfismo ad hoc si importa el tipo porque eso te dice dentro de que clases de tipos esta y que cosas puede hacer
-}
data Dia = Lunes | Martes | Miercoles | Jueve | Viernes | Sabado | Domingo

data ListaDeEnteros = Vacia | Agregar Int ListaDeEnteros --tipo de dato infinito
--tiene que ser "inductiva" osea, tener un elemento inicial y una forma de seguir con un patron

cantidad :: ListaDeEnteros -> Int
cantidad Vacia = 0
cantidad (Agregar x xs) = 1 + cantidad xs

identidad :: a -> a
identidad x = x

instance Eq Dia where --esto es lo mismo que deriving Eq en la declaracion del constructor
    (==) Lunes Lunes = True
    (==) Lunes _ = False
    (==) Martes Martes = True
    (==) Martes _ = False {-
    (==) Lunes Lunes = True
    (==) Lunes _ = False
    (==) Lunes Lunes = True
    (==) Lunes _ = False
    (==) Lunes Lunes = True
    (==) Lunes _ = False
    (==) Lunes Lunes = True
    (==) Lunes _ = False
    (==) Lunes Lunes = True
    (==) Lunes _ = False -}

instance Eq ListaDeEnteros where --(==) :: ListaDeEnteros -> ListaDeEnteros -> Bool --esto es lo mismo que deriving Eq en el constructor
  (==) Vacia Vacia = True
  (==) Vacia l = False
  (==) (Agregar a l) Vacia = False
  (==) (Agregar a l) (Agregar b l') = a == b && l == l'

------------------------------------------------------{ 3 }-------------------------------------------------------------------------------
--a hecho en la actividad 2 c
{-
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si 
  deriving Ord
  deriving Bounded -}

------------------------------------------------------{ 4 }-------------------------------------------------------------------------------
--a
{-
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) =
  let minRest = minimoElemento xs
  in if x <= minRest then x else minRest
-}
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

minimoElemento' :: (Ord a, Bounded a, Show a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)


------------------------------------------------------{ 5 }-------------------------------------------------------------------------------
--a
type Altura = Int
type NumCamiseta = Int

data Zona = Arco | Defensa | Mediocampo | Delantera   deriving Eq deriving Ord deriving Show
data TipoReves = DosManos | UnaMano deriving Eq deriving Ord deriving Show
data Modalidad = Carretera | Pista | Monte | BMX deriving Eq deriving Ord deriving Show
data PiernaHabil = Izquierda | Derecha deriving Eq deriving Ord deriving Show

type ManoHabil = PiernaHabil

data Deportista = Ajedrecista | Ciclista Modalidad | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura
  deriving Eq
  deriving Ord
  deriving Show

--b
--el tipo de constructor con un solo parametro? Parametrico? deportista int??? tupla de datos???

--c 
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas ((Velocista x):xs) = 1 + contar_velocistas xs
contar_velocistas (x:xs) = 0 + contar_velocistas xs

contarVelocistasAltos :: [Deportista] -> Int
contarVelocistasAltos [] = 0
contarVelocistasAltos ((Velocista x):xs)
  | x >= 170  = 1 + contarVelocistasAltos xs
  | otherwise = contarVelocistasAltos xs
contarVelocistasAltos (_:xs) = contarVelocistasAltos xs

--d
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas ((Futbolista x y w v):xs) z | z==x = 1+contar_futbolistas xs z
                                               | otherwise = contar_futbolistas xs z
contar_futbolistas (_:xs) z = contar_futbolistas xs z

--e
contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' [] _ = 0
contar_futbolistas' xs z = length (filter esFutbolistaEnZona xs)
  where esFutbolistaEnZona :: Deportista -> Bool
        esFutbolistaEnZona (Futbolista zona _ _ _) = z == zona
        esFutbolistaEnZona _ = False

--------------------------------------------------{ 6 }------------------------------------------------------------------------------------
--a
sumPotencias :: Int -> Int -> Int
sumPotencias _ 0 = 1
sumPotencias x i = x^i + sumPotencias x (i-1)

--b
{-
pi :: Int -> Float
pi 0 = (-1) ** 0 / (2 * 0 + 1)
pi i = 4* ( (((-1) ** fromIntegral i)/fromIntegral (2* i + 1 )) + pi (i-1) )

piAprox :: Int -> Float
piAprox 0 = 0
piAprox n = 4 * sumatoria n

sumatoria :: Int -> Float
sumatoria 0 = (-1) ** 0 / (2 * 0 + 1)
sumatoria i = (-1) ** fromIntegral i / fromIntegral (2 * i + 1) + sumatoria (i - 1)
-}

pi' :: Int -> Float
pi' n = 4 * sumatoriaDePi (n-1)
  where
    sumatoriaDePi :: Int -> Float
    sumatoriaDePi 0 = 1 -- El primer término es 1, cuando i = 0
    sumatoriaDePi x = exp'' x + sumatoriaDePi (x - 1)

    exp'' :: Int -> Float
    exp'' z = ((-1) ** fromIntegral z) / fromIntegral (2 * z + 1)

--c

-------------------------------------------------------{ 11 }------------------------------------------------------------------------------

--------------------------------------------------[ clase 16/09/24 ]-----------------------------------------------------------------------
--PRE: la lista es no vacia
primerElemento :: [a] -> a
primerElemento (x:xs) = x
--prelude data type, no need to redefine it, possible cause of errors
--data Maybe a = Nothing | Just a

-- devuelve Nothing si la lista es vacia o just y el primer elemento
primerElemento' :: [a] -> Maybe a
primerElemento' [] = Nothing
primerElemento' (x:xs) = Just x

data Cola = VaciaC | Encolada Deportista Cola
  deriving Eq
  deriving Show

fromJust :: Maybe a -> a
fromJust (Just x) = x

unaCola :: Cola
unaCola = Encolada (Velocista 170)  (Encolada Ajedrecista VaciaC)

--funcionamiento: saca al ultimo de la cola, que es el que es atendido
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada d VaciaC) = Just VaciaC
atender (Encolada d (Encolada d' c)) = Just (Encolada d (fromJust  (atender (Encolada d' c)))) --tambien se puede usar la funcion del where envez del fromJust
  where atenderColaNoVacia :: Cola -> Cola
        atenderColaNoVacia (Encolada d' VaciaC) = VaciaC
        atenderColaNoVacia (Encolada d' c) = Encolada d' (atenderColaNoVacia c)

--version considerando que se saca al deportista del incio

atender' :: Cola -> Maybe Cola
atender' VaciaC = Nothing
atender' (Encolada d c) = Just c
-------------------------------------------------------{ 12 }------------------------------------------------------------------------------
--b 
--mete al final de la cola a alguien (pero como la anterior funcion tomaba al del final la anterior funcion parece una pila xd)
encolar :: Deportista -> Cola -> Cola
encolar d VaciaC = Encolada d VaciaC
encolar d (Encolada d' c) = Encolada d' (encolar d c)
-------------------------------------------------------{ 13 }------------------------------------------------------------------------------
--a

--type ListaAsoc = [(String,Int)] esta solucion no es la idea
data ListaAsoc a b = Vacia' | Nodo a b (ListaAsoc a b) 

ej :: ListaAsoc String Int
ej = Nodo "Bs As" 20000000 (Nodo "Cba" 4000000 Vacia')

--e
buscar :: Eq a => ListaAsoc a b -> a -> Maybe b
buscar Vacia' _ = Nothing
buscar (Nodo k v la) k' | k == k' = Just v
                        | otherwise = buscar la k'

--f
borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
borrar _ Vacia' = Vacia'
borrar k' (Nodo k v la) | k' == k   = borrar k' la
                        | otherwise = Nodo k v (borrar k' la)
                        
