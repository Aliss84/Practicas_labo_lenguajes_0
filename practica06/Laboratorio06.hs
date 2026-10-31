module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0            = 1
potencia n m =
    n * potencia n (m - 1) -- aqui preferiria que usaramos pred, pero creo nos dijeron que no usaramos nada de lo que está en el Prelude (basicamente cualquier función nativa que no sean (+), (*), (:) de acuerdo a: https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html)
 -- pred :: Enum a => a -> a; base Prelude Data.Enum GHC.Enum, ghc GHC.Prelude.Basic, ghc-internal GHC.Internal.Enum, incipit-base Incipit.Bas; ref: https://hoogle.haskell.org/?hoogle=pred

potenciaCola :: Int -> Int -> Int
potenciaCola n m        = potenciaCola' n m 1
    where -- este uso de where es curioso
        potenciaCola' :: Int -> Int -> Int -> Int
        potenciaCola' _ 0 acc = acc -- si no hay nada que multiplicar, entonces se devuelve el acumulador
        potenciaCola' n m acc = potenciaCola' n (m - 1) (acc * n) -- para cuando m = 0, se guarde el valor

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1 -- suponiendo que sea cierto que para todo a^0 = 1, k captura el valor y devuelve lo que capture k (que supongo por el tipo, r :: Int y así k :: Int -> Int)
potenciaCPS n m k = potenciaCPS n (m - 1) (\res -> k (n * res)) -- aqui llamamos a la funcion recursivamente t como k es una función entonces se vale una lambda, en este caso k captura al resultados de la recursión

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud []         = 0
longitud (_ : xs)   = 1 + longitud xs

longitudColaExtra :: [a] -> Int -> Int
longitudColaExtra [] acc        = acc
longitudColaExtra (x:xs) acc    = longitudColaExtra xs (1 + acc)

longitudCola :: [a] -> Int
longitudCola xs     = longitudColaExtra xs 0

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k        = k 0
longitudCPS (x:xs) k    = longitudCPS xs (\v -> k (1 + v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x] -- no es algo personal, pero prefiero que las listas se concatenan usando el operando (:), porque aveces con listas con clase de tipo a, puede dar errores :(

reversaColaExtra :: [a] -> [a] -> [a]
reversaColaExtra [] lista       = lista
reversaColaExtra (x:xs) lista   = reversaColaExtra (xs) (x:lista)

reversaCola :: [a] -> [a]
reversaCola y = reversaColaExtra y []

reversaCPS :: [a] -> ([a] -> r) -> r 
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k(x:v))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaColaExtra :: [[a]] -> [a] -> [a]
concatenaColaExtra [] lista = lista
concatenaColaExtra (x:xs) lista = concatenaColaExtra xs (lista++x)

concatenaCola :: [[a]] -> [a]
concatenaCola [] = concatenaColaExtra [] [] 
concatenaCola (xs : xss) = concatenaColaExtra (xs : xss) [] 

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k = concatenaCPS xss (\v -> k(xs++v)) 

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
    x : y : intercala xs ys

intercalaColaExtra :: [a] -> [a] -> [a] -> [a] 
intercalaColaExtra [] y lista = lista++y
intercalaColaExtra x [] lista = lista++x
intercalaColaExtra (x:xs) (y:ys) lista = intercalaColaExtra xs ys (lista++[x,y])

intercalaCola :: [a] -> [a] -> [a]
intercalaCola [] [] = intercalaColaExtra [] [] []
intercalaCola [] (y:ys) = intercalaColaExtra [] (y:ys) []
intercalaCola (x:xs) [] = intercalaColaExtra (x:xs) [] []
intercalaCola (x:xs) (y:ys) = intercalaColaExtra (x:xs) (y:ys) []

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r 
intercalaCPS [] [] k = k []
intercalaCPS [] (y:ys) k = k (y:ys)
intercalaCPS (x:xs) [] k = k (x:xs)
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k(x:y:v))

