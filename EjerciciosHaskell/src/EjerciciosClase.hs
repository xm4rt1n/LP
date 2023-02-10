module EjerciciosClase where

    import Data.Char
    
    --------------------------------------------------- Tema 3 ---------------------------------------------------

    -- funcion que dados tres numeros enteros devuelve el mayor de ellos 
    -- utilizando otra funcion que calcula el maximo entre dos numeros enteros
    maximo2Num :: Int -> Int -> Int
    maximo2Num x y
        | x < y = y
        | otherwise = x

    maximo3Num :: Int -> Int -> Int -> Int
    maximo3Num x y = maximo2Num x.maximo2Num y

    -- funcion que dados dos numeros devuelve una lista con el resutlado de sumar, restar y multiplicar ambos numeros
    listaSRM :: Int -> Int -> [Int]
    listaSRM x y = [x+y, x-y, x*y]

    -- funcion que dadas dos cadenas de caracteres ddevuelven la unicion si la longitud es menor o igual a tres
    -- si no se cumple esta condicion se devuelve una cadena vacia
    union :: [Char] -> [Char] -> [Char]
    union x y = if length x <= 3 && length y <= 3 then x++y else []

    -- funcion que dada una lista de enteros devuelve otra lista con el cuadrado de cada uno de sus elementos
    cuadradoLista :: [Int] -> [Int]
    cuadradoLista list = [x*x| x <- list]

    -- funcion que dada una frase devuelve una cadena con los caracteres que estaban en mayusculas
    devolverMayus :: [Char] -> [Char]
    devolverMayus list = [x| x <- list, isUpper x]