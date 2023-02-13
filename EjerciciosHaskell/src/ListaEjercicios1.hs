module ListaEjercicios1 where
    
    ----------------------------------------------- Hoja de ejercicios 1 -----------------------------------------------

    -- Funcion que comprueba si estan ordeandos tres numeros dados
    estanOrdenados :: Int -> Int -> Int -> Bool
    estanOrdenados x y z = (x < y) && (y < z)

    -- Funcion que dada una tupla la ordena de menor a mayort
    ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
    ordenarTupla (x, y, z)
        | x < y && y < z             = (x, y, z)
        | x < y && y > z && x < z    = (x, z, y)
        | x > y && y > z             = (z, y, x)
        | x > y && x < z             = (y, x, z)
        | x > y && x > z && y < z    = (y, z, x)
        | otherwise                  = (z, x, y)

    -- Funcion que dado un decimal devuelve una tupla de dos elementos, su parte entera y sus dos primeros decimales (como numero entero)
    descomponerReal :: Float -> (Int, Int) 
    descomponerReal x = (truncate(x), mod (truncate(x*100)) 100)

    -- Funcion que dado un entero devuelve una lista de enteros que contiene todos los divisores de dicho numero
    divisores :: Int -> [Int]
    divisores n = [x| x <-[1..n], n `mod` x == 0]

    -- Funcion que devuelve si un caracter es un digito utilizando guardas
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
       
    -- Funcion que devuelve si un caracter es un digito utilizando patrones
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