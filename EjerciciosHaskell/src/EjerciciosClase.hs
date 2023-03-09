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
    devolverMayus list = [x | x <- list, isUpper x]

    --------------------------------------------------- Tema 4 ---------------------------------------------------

    -- funcion que calcula el tamaño de una lista recursivamente con una unica ecuacion
    longitud :: [Int] -> Int
    longitud lista = if lista == [] then 0 else 1+longitud (tail lista)
    
    -- funcion que calcula el tamaño de una lista recursivamente con patrones
    longitud' :: [Int] -> Int
    longitud' [] = 0
    longitud' (_:xs) = 1 + longitud' xs

    -- funcion recursiva no final que dadas una cadena y un caracter cuente el numero de veces que aparece el caracter en la cadena
    contarApariciones :: String -> Char -> Int
    contarApariciones [] _ = 0
    contarApariciones (x:xs) c = if (x==c) then 1 + contarApariciones xs c else contarApariciones xs c

    -- funcion recursiva final que dadas una cadena y un caracter cuente el numero de veces que aparece el caracter en la cadena
    contarApariciones' :: String -> Char -> Int
    contarApariciones' cadena c = contarAparicionesAux cadena c 0
        -- creo una funcion auxiliar para pasarle un parametro de acumulacion en el cual voy a ir sumando y asi hacer que la 
        -- recursividad sea final.
    contarAparicionesAux :: String -> Char -> Int -> Int     
    contarAparicionesAux [] _ contador = contador
    contarAparicionesAux (x:xs) c contador = if (x==c) then contarAparicionesAux xs c (contador+1) else 
        contarAparicionesAux xs c contador

    -- funcion recursiva no final que dada una lista de numeros devuelve la suma de todos ellos
    sumaLista :: [Int] -> Int
    sumaLista [] = 0 
    sumaLista (x:xs) = x + sumaLista xs 

    -- funcion recursiva final que dada una lista de numeros devuelve la suma de todos ellos
    sumaLista' :: [Int] -> Int
    sumaLista' lista = sumaListaAux lista 0

    sumaListaAux :: [Int] -> Int -> Int
    sumaListaAux [] cont = cont
    sumaListaAux (x:xs) cont = sumaListaAux xs (cont+x)

    -- funcion que calcule la longitud de una lista utilizando funciones de plegado y funciones anonimas
    longitud'':: [Int] -> Int
    longitud'' = foldr (\_ n -> n+1) 0

    -- funcion que reciba una lista de funciones que se aplican a un segundo argumento de tipo enteros
    -- y retorna una lista de numeros enteros con el resultado de cada funcion. 
        -- ejercicio realizado con patrones
    listaFunciones:: [Int -> Int] -> Int -> [Int]
    listaFunciones [] _ = []
    listaFunciones (f:fs) n = f n : listaFunciones fs n

        -- ejercicio realizado con listas de compresion
    listaFunciones':: [Int -> Int] -> Int -> [Int]    
    listaFunciones' lista n = [f n| f <- lista]

        -- ejercicio utilizando funciones de plegado
    listaFunciones'':: [Int -> Int] -> Int -> [Int]
    listaFunciones'' lista n = foldr (\f ac -> [f n] ++ ac) [] lista

    
    --------------------------------------------------- Tema 5 ---------------------------------------------------
    data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving (Show, Eq)

    a1 = Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV))) 123 (Rama AV 10 AV)
    a2 = Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV))) 123 (Rama AV 10 AV)

    hojas :: Arbol a -> [a]
    hojas AV = []
    hojas (Rama AV h AV) = [h]
    hojas (Rama izq r der) = hojas izq ++ hojas der
    -- posicion :: [Int] -> Int -> Maybe Int
    -- posicion lista num = posicionAux lista num 0
    
    -- Dada la siguiente declaracion de arbol polimorfico, se pide implementar una funcion que diga si dos arboles diga si son iguales
   

    arbolesIguales :: (Eq a) => Arbol[a] -> Arbol[a] -> Bool
    arbolesIguales AV AV = True
    arbolesIguales AV _ = False
    arbolesIguales _ AV = False
    arbolesIguales (Rama i1 r1 d1) (Rama i2 r2 d2) = r1 == r2 && arbolesIguales i1 i2 && arbolesIguales d1 d2

    -- si se hace creando una instancia de la clase Eq para el tipo arbol, se estaria implementando la funcion == para comparar arbolesIguales
    --instance (Eq a) => Eq (Arbol a) where
    --    AV == AV == True
    --    AV == _ = False
    --    _ == AV = False
    --    (Rama i1 r1 d1) == (Rama i2 r2 d2) 


    -- se pide una funcion polimorfica que dada una lista de elem devuelva el menor
    menorPolimorfica :: Ord a => [a] -> Maybe a
    menorPolimorfica [] = Nothing
    menorPolimorfica[x] = Just x
    menorPolimorfica (x:y:xs) =  menorPolimorfica ((min x y):xs)

    -- se pide una funcion que dado un elemento y una lista de tuplas de dos elementos, busque la tupla donde el elemtno
    -- dado esta en primer lugar y devuelva su pareja en la tupla.
    --buscarPareja :: Eq a => a -> [(a, b)] -> Maybe b
    --buscarPareja x [] = Nothing
    --buscarPareja x [(m, n):xs] = 