module ListaEjercicios0 where

    ----------------------------------------------- Hoja de ejercicios 0 -----------------------------------------------

    -- funcion que nos da el mayor entre el resto y el cociente entre x y n
    componer :: (Int, Int) -> Int
    componer (x, n) 
        | (x `mod` n) > (x `div` n) = x `mod` n
        | otherwise = x `div` n

    -- funcion que nos da el sucesor al nunero introducido
    sucesor :: Int -> Int
    sucesor x = x+1    

    -- funcion que nos devuelve el doble de un numero dado
    doble :: Int -> Int
    doble x = 2*x

    -- funcion que nos devuelve el cuadruple de un numero dado llamando dos veces a la funcion doble
    cuadruple :: Int -> Int
    cuadruple x = doble(doble x)

    -- funcion que nos devuele el mayor de los numeros introducidos
    mayor :: (Int, Int) -> Int
    mayor (x, y)
        | x > y = x
        | otherwise = y

  