module Ejercicios1 where
    
    ----------------------------------------------- Hoja de ejercicios 0 -----------------------------------------------

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

    