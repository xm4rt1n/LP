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

        
    -- 3. 

    -- 4.


    -- 5. Sumar las cifras de un entero

    separarDigitos :: Int -> [Int]
    separarDigitos n
        | n == 0 = []
        | otherwise = separarDigitos (n`div`10) ++ [n `mod`10] 

    sumarCifras:: Int -> Int
    sumarCifras n = foldr (+) 0 (separarDigitos n)

    -- 6. Dados una cifra y un numero devolver si el numero contiene esa cifra

    contieneCifra:: Int -> Int -> Bool
    contieneCifra c n = foldr (\x ac -> if c == x then True else ac) False (separarDigitos n) 

    -- 7. Dado un numero invertir sus cifras

    unirDigitos :: [Int] -> Int
    unirDigitos xs = joinDigits xs 0

    joinDigits :: [Int] -> Int -> Int
    joinDigits [] acc     = acc
    joinDigits (x:xs) acc = joinDigits xs (acc * 10 + x)

    invertir:: Int -> Int
    invertir x = unirDigitos(reverse(separarDigitos x))

    -- 8. Dado un entero n y una lista elimina los n ultimos enteros de la lista.
        -- Usando funciones predefinidias
    eliminarUltimos :: Int -> [a] -> [a]
    eliminarUltimos n xs = reverse (drop n (reverse xs))
        
        -- Usando recursividad no final
    

    -- 9. Dada una lista de enteros devuelve si sus elementos estan ordenados de mayor a menor.
    listaOrdenada:: [Int] -> Bool
    listaOrdenada lista = listaOrdenadaAux lista 0

    listaOrdenadaAux:: [Int] -> Int -> Bool
    listaOrdenadaAux [] _ = True
    listaOrdenadaAux (x:xs) ac = if x > ac then True && listaOrdenadaAux xs x else False

 
    --------------------------------------------------- Lista II ---------------------------------------------------

    -- 2. Dada una lista de enteros obtiene el resultado de calcular el doble de cada uno de los elementos y sumarlos todos
        -- con recursividad no final
    sumaDobles:: [Int] -> Int        
    sumaDobles [] = 0
    sumaDobles (x:xs) = x*2 + sumaDobles xs
        -- con recursividad final
    sumaDobles':: [Int] -> Int 
    sumaDobles' lista = sumaDoblesAux lista 0

    sumaDoblesAux:: [Int] -> Int -> Int
    sumaDoblesAux [] ac = ac
    sumaDoblesAux (x:xs) ac = sumaDoblesAux xs (x*2 +ac)     
        -- utilizando foldr
    sumaDobles'':: [Int] -> Int 
    sumaDobles'' lista = foldr (\x ac -> x*2 +ac) 0 lista

    -- 3. Funcion que suma los cuadrados de los numeros de los numeros pares de una lista de numeros
        -- usando fold map y filter
    sumaCuadradosPares:: [Int] -> Int
    sumaCuadradosPares lista = foldl (+) 0  (map (^2) (filter even lista))
    
        -- usando listas por comprension
    sumaCuadradosPares':: [Int] -> Int
    sumaCuadradosPares' list = sum [x^2 | x <- list, even x]

    -- 4. dado un numero y una lista de numeros elimina las aparicioneas de dicho numero usando fold
    eliminaValor:: Int -> [Int] -> [Int]
    eliminaValor n lista = foldr (\x ac -> if x == n then ac else [x]++ ac) [] lista

    eliminaValor':: Int -> [Int] -> [Int]
    eliminaValor' n lista = foldl (\ac x -> if x == n then ac else ac ++ [x]) [] lista

    -- 5. dada una liosta eliminar los duplicados usando foldl
    eliminaDuplicados:: [Int] -> [Int]
    eliminaDuplicados = foldl (\ac x -> if x `elem`ac then ac else ac ++ [x]) []

    -- 6. Dada una lista devolver otra que contenga solamente los primos
    divisores :: Int -> [Int]
    divisores n = [x | x <-[1..n], n `mod` x == 0]

    esPrimo:: Int -> Bool
    esPrimo n = if  divisores n == [1,n] || n==1 then True else False
    
    listaPrimos:: [Int] -> [Int]
    listaPrimos [] = []
    listaPrimos (x:xs) = if esPrimo x then [x] ++ listaPrimos xs else listaPrimos xs

        -- usando filter
    listaPrimos':: [Int] -> [Int]
    listaPrimos' list = filter (esPrimo) list


    --------------------------------------------------- Lista III ---------------------------------------------------

    -- 2. Funcion polinomica que dado un elemnto y una lista lo añada al final.
        -- con ajuste de patrones.
    alFinal:: a -> [a] -> [a]
    alFinal e [] = [e]
    alFinal e (x:xs) = x: alFinal e xs

        -- con foldr
    alFinal':: a -> [a] -> [a]
    alFinal' e lista = foldr (:) [e] lista

    -- 3. Funcion polimorfica que dada una funcion y una lista vatya recorriendo la lista y devuelva los elementos mientras que f sea true.

    takeWhile':: (a -> Bool) -> [a] -> [a]
    takeWhile' _ [] = []
    takeWhile' f (x:xs)
        | f x = x: takeWhile' f xs
        | otherwise = []


    -- 4. Funcion polimorfica que dada una dupla compuesta por un elemento y una lista de elementos devuevla las posiciones en las que esta ese eleemtnso. Usando foldl
    posicionesElem :: Eq a => (a, [a]) -> [Int]
    posicionesElem (x, xs) = foldl (\acc (y, i) -> if x == y then i : acc else acc) [] (zip xs [1..])

    -- 5. Funcion polinomica que dado un elemento e y una lista de elementos devuevla true si dicho elemento esta en la listaOrdenada
    contiene:: Eq a => a -> [a] -> Bool
    contiene e lista = foldr (\x acc -> if x == e then True else acc) False lista

    contiene':: Eq a => a -> [a] -> Bool
    contiene' e lista = foldl (\ac x-> if x == e then True else ac) False lista

    