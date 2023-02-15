module ListaEjercicios1 where
    import Data.Char
    
    ----------------------------------------------- Hoja de ejercicios 1 -----------------------------------------------

    -- 1. Funcion que comprueba si estan ordeandos tres numeros dados
    estanOrdenados :: Int -> Int -> Int -> Bool
    estanOrdenados x y z = (x < y) && (y < z)

    -- 2. Funcion que dada una tupla la ordena de menor a mayort
    ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
    ordenarTupla (x, y, z)
        | x < y && y < z             = (x, y, z)
        | x < y && y > z && x < z    = (x, z, y)
        | x > y && y > z             = (z, y, x)
        | x > y && x < z             = (y, x, z)
        | x > y && x > z && y < z    = (y, z, x)
        | otherwise                  = (z, x, y)

    -- 3. Funcion que dado un decimal devuelve una tupla de dos elementos, su parte entera y sus dos primeros decimales (como numero entero)
    descomponerReal :: Float -> (Int, Int) 
    descomponerReal x = (truncate(x), mod (truncate(x*100)) 100)

    -- 4. Funcion que dado un entero devuelve una lista de enteros que contiene todos los divisores de dicho numero
    divisores :: Int -> [Int]
    divisores n = [x | x <-[1..n], n `mod` x == 0]

    -- 5a. Funcion que devuelve si un caracter es un digito utilizando guardas
    esDigito :: Char -> Bool
    esDigito c
        | c == '1' = True
        | c == '2' = True
        | c == '3' = True
        | c == '4' = True
        | c == '5' = True
        | c == '6' = True
        | c == '7' = True
        | c == '8' = True
        | c == '9' = True
        | c == '0' = True
        | otherwise = False
       
    -- 5b. Funcion que devuelve si un caracter es un digito utilizando patrones
    esDigito' :: Char -> Bool
    esDigito' '1' = True
    esDigito' '2' = True
    esDigito' '3' = True
    esDigito' '4' = True
    esDigito' '5' = True
    esDigito' '6' = True
    esDigito' '7' = True
    esDigito' '8' = True
    esDigito' '9' = True
    esDigito' '0' = True
    esDigito' _ = False

    -- 6. Funcion que dice si un nuemro es primo utilizando la funcion que calcula divisores
    esPrimo:: Int -> Bool
    esPrimo n = if  divisores n == [1,n] || n==1 then True else False

    -- 7. Funcion que dada una lista de enteros devuvle otra lista con los elementos de esa lista que son impares y primos
    listaPrimosImpares :: [Int] -> [Int]
    listaPrimosImpares list = [x | x <- list, esPrimo x, x`mod`2/=0]

    -- 8. Funcion que dado un entero devuelve una lista con numeros primos menores iguales o menores a dicho numero
    primosMenorIgual :: Int -> [Int]
    primosMenorIgual n = [x | x <- [1..n], esPrimo x]

    -- 9. 
    esVocal:: Char -> Bool
    esVocal c = if c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' || c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' then True else False

    --codificacionTuplas :: [(Char, Char)] -> String
    --codificacionTuplas [(x,y) :xs] = [x | x <- xs , y <- c2, esVocal y]

    --10

    

    -- 12. 
    esMayuscula :: Char -> Bool
    esMayuscula c = if ord c >= 65 && ord c <= 90 then True else False

    -- 13. 
    cambiarTipo :: Char -> Char
    cambiarTipo c = if ord c > 90 then chr(ord c - 32) else chr(ord c + 32)

    mayusculasMinusculas :: String -> String
    mayusculasMinusculas list = [cambiarTipo x| x <- list]

    -- 14
    listaASCII :: String -> [Int]
    listaASCII list = [ord x| x <- list]

    -- 15
    mensajeLista :: [Int] -> String
    mensajeLista lista = "Primer elemento: " ++ [intToDigit(head lista)] ++ ", numero de elementos: " ++ [intToDigit( length lista)]

    -- 16
    contarMayusculas :: String -> Int
    contarMayusculas lista = length [x | x <- lista, esMayuscula x]