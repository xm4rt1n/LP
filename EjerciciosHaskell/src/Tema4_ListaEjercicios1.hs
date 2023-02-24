module Tema4_ListaEjercicios1 where

    -- 1. funcion que dado un entero y una lista de enteros elimine de la lista los numeros del entero
        -- usando listas de compresion
    cribar :: [Int] -> Int -> [Int]
    cribar lista n = [x | x<- lista, x `mod` n /= 0]

            -- usando recursividad no final
    cribar' :: [Int] -> Int -> [Int]
    cribar' [] _ = []
    cribar' (x:xs) n = if x `mod`n == 0 then cribar' xs n else x: cribar' xs n
            
            -- usando recursividad final
    cribar'' :: [Int] -> Int -> [Int]
    cribar'' lista n = cribarAux lista n []
    cribarAux :: [Int] -> Int -> [Int] -> [Int]
    cribarAux [] _ acumulador = acumulador 
    cribarAux (x:xs) n acumulador = if x `mod`n == 0 then cribarAux xs n  acumulador else x: cribarAux xs n acumulador

    -- 2. funcion que  calcule el numero de secuencias de ceros que hay en una lista de numeros 
    ceros :: [Int] -> Int
    ceros [] = 0
    ceros [0] = 1
    ceros lista = cerosAux lista 1

    cerosAux :: [Int] -> Int -> Int
    cerosAux [] _ = 0
    cerosAux (x:xs) n
        | n == 0 = cerosAux xs x 
        | n /= 0 && x == 0 = 1+ cerosAux xs x
        | otherwise = cerosAux xs x

        