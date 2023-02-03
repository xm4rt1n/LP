module Ejercicio where

    hayInterseccion :: (Int, Int) -> (Int, Int) -> Bool
    hayInterseccion (x,y) (m,n) = x == y

    mayor :: (Int, Int) -> Int
    mayor (x, y)
        | x > y = x
        | otherwise = y

  