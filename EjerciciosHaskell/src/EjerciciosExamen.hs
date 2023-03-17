module EjerciciosExamen where

-- 1.
pertenece:: Eq a => a -> [a] -> Bool
pertenece n lista = foldr (\x acc -> if x == n then True else acc) False lista

-- 2.
eliminar:: Eq a => a -> [a] -> [a]
eliminar n lista = foldr (\x acc -> if x == n then acc else [x]++acc) [] lista

-- 3. 

productoEscalar:: Real a => [a] -> [a] -> a
-- productoEscalar a b = foldl (\acc (xa, xb) -> xa*xb + acc) 0 (zip a b)
productoEscalar a b = foldl (+) 0 [x*y | (x,y)<-(zip a b)]


--------------------------------------------------- Lista II ---------------------------------------------------

-- 1. 
type Marca = String
type Numero = Int
type Deporte = String
data Calzado = Bota Marca Numero | Zapatilla Marca Numero Deporte

    -- b
instance Show Calzado where
    show (Bota m n) =  "Bota de la marca " ++ m ++ " del numero " ++ show n ++ "\n"
    show (Zapatilla m n d) = "Zapatilla de la marca " ++ m ++ " del numero " ++ show n ++ " para " ++ d ++ "\n"

    -- c
 
-- 2. 
data Version = V {
    major:: Integer,
    minor:: Integer
}
data Libreria = Lib {
    nombre:: String, 
    version:: Version
}

instance Show Version where
    show (V ma mi) = show ma ++ "." ++ show mi

instance Show Libreria where
    show (Lib n v) = n ++ " " ++ show v
    --b
instance Eq Version where
    (V ma mi)==(V m2 mi2) = ma == m2 && mi ==mi2

instance Ord Version where
    v1 < v2 = major v1 < major v2 || major v1 == major v2 && minor v1 < minor v2
    v1 <= v2 = major v1 < major v2 ||  major v1 == major v2 && minor v1 <= minor v2 
    v1 > v2 = major v1 > major v2 ||  major v1 == major v2 && minor v1 > minor v2    
    v1 >= v2 = major v1 > major v2 ||  major v1 == major v2 && minor v1 >= minor v2 

instance Eq Libreria where
    l1 == l2 = nombre l1 == nombre l2 && version l1 == version l2
    
instance Ord Libreria where    
    l1 > l2 = (nombre l1 > nombre l2) || (nombre l1 == nombre l2 && version l1 > version l2)     
    l1 < l2 = (nombre l1 < nombre l2) || (nombre l1 == nombre l2 && version l1 < version l2)     
    l1 <= l2 = (nombre l1 <= nombre l2) || (nombre l1 == nombre l2 && version l1 <= version l2)     
    l1 >= l2 = (nombre l1 >= nombre l2) || (nombre l1 == nombre l2 && version l1 >= version l2) 

    -- c
class Compatible a where
    compatible:: a -> a -> Bool

instance Compatible Libreria where    
    compatible l1 l2 = nombre l1 == nombre l2 && major (version l1) == major (version l2)

    -- d
libreriasCompatibles:: Libreria -> [Libreria] -> [Libreria]
libreriasCompatibles l lista =[comp | comp <-lista, compatible comp l]
--------------------------------------------------- Lista III ---------------------------------------------------

--2.
separarVocales:: String -> (String, String)
separarVocales lista = foldr (\c (b1, b2) -> if (c=='a')||(c=='e')||(c=='i')||(c=='o')||(c=='u') then (c:b1, b2) else (b1, c:b2)) ([],[]) lista

--3. 
separarPorPosicion:: [a] -> ([a], [a])
separarPorPosicion lista = foldr (\(n, p) (ac1, ac2)-> if (odd p) then (ac1, n:ac2) else (n:ac1, ac2)) ([],[]) (zip lista [1..])

-- 4.
p :: [a] -> [[a]] 
p [] = [[]] 
p (x:xs) = []: map (x:) (p xs)


data List a = Vacia | Cons a (List a) deriving Show
l1 :: List Int 
l1 = Cons 1 (Cons 2 (Cons 3 Vacia))  
l2 :: List Int 
l2 = Cons 10 (Cons 20 (Cons 30 Vacia))

sumal::Num a => List a-> List a-> List a
sumal Vacia Vacia = Vacia
sumal l Vacia = l
sumal Vacia l = l
sumal (Cons n l) (Cons n2 lis) = Cons (n+n2) (sumal l lis)