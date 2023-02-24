module Tema3_ListaEjercicios2 where

    -- 1. funcion que dada una cadena de caracteres y un caracter indique el numero de apariciones de este en la cadena
    contarCaracter :: String -> Char -> Int
    contarCaracter [] _ = 0
    contarCaracter cadena c = length[x | x<- cadena, x==c]

    -- 2. funcion que dada una 3-tupla donde cada elemento es a su vez una tupla de dos elementos retorne una 3-tupla que contiene el primer elemento de cada tupla
    manipula3Tuplas :: ((String, Int),(String, Int),(String, Int)) -> (String, String, String)
    manipula3Tuplas ((a, _), (b, _), (c, _)) = (a, b, c)

    -- 3. funcion que devuelve true si la suma de los cuatro primeros elementos de la lista de enteros es menor a 10, devuelve fasle de lo contrario
    sumaMenor10 :: [Int] -> Bool
    sumaMenor10 (a:b:c:d:_) = (a + b + c + d) < 10
    sumaMenor10 _ = False
