module Tema5_ListaEjercicios where

type Nombre = String  
type Edad = Integer 
type Persona = (Nombre, Edad)

-- 1. dada una perosna determinar si puede tener el abono (es joven o jubilado)
descuentoAbono:: Persona-> Bool
descuentoAbono (_, edad)
    | edad < 18 = True
    | edad > 65 = True
    | otherwise = False


-- 2. definir un tipode datos alumno con sinonimos de tiupos
type DNI = String
type NExp = Int
type Nota = Float
type Alumno = (DNI, NExp, Nota)
    -- implementar una funciuon que deternine si un alumno ha aprobado.
aprobado:: Alumno -> Bool
aprobado (_, _, nota)
    | nota>=5 = True
    | otherwise = False

    -- implementar una fucnion que dado el alumno muestre un mensaje con su calificacion.
calificacionAlumno :: Alumno -> String
calificacionAlumno (_, ne, nota)
    | nota < 5 = "Expediente " ++ show ne ++ ", Nota Acta: Suspenso"
    | nota < 6 = "Expediente " ++ show ne ++ ", Nota Acta: Suficiente"
    | nota < 7 = "Expediente " ++ show ne ++ ", Nota Acta: Bien"
    | nota < 9 = "Expediente " ++ show ne ++ ", Nota Acta: Notable"
    | otherwise = "Expediente " ++ show ne ++ ", Nota Acta: Sobresaliente"

    -- funcion que devuelve la nota
dameNota:: Alumno -> Nota
dameNota (_, _, nota) = nota

    -- funcion que dada una lista de alumnos devuelva la nota media.
mediaNotas:: [Alumno] -> Float
mediaNotas [] = 0
mediaNotas lista = (notasAux lista) / fromIntegral(length lista)

notasAux:: [Alumno] -> Nota
notasAux [] = 0
notasAux ((_, _, nota):xs) = nota + notasAux xs

-- 3. definir el tipo de dato alumno usando tipos producto

data Alumno' = Alumno DNI NExp Nota
aprobado':: Alumno' -> Bool
aprobado' (Alumno _ _ nota)
    | nota>=5 = True
    | otherwise = False

calificacionAlumno' :: Alumno' -> String
calificacionAlumno' (Alumno _ ne nota)
    | nota < 5 = "Expediente " ++ show ne ++ ", Nota Acta: Suspenso"
    | nota < 6 = "Expediente " ++ show ne ++ ", Nota Acta: Suficiente"
    | nota < 7 = "Expediente " ++ show ne ++ ", Nota Acta: Bien"
    | nota < 9 = "Expediente " ++ show ne ++ ", Nota Acta: Notable"
    | otherwise = "Expediente " ++ show ne ++ ", Nota Acta: Sobresaliente"    

dameNota':: Alumno' -> Nota
dameNota' (Alumno _ _ nota) = nota    

mediaNotas':: [Alumno'] -> Float
mediaNotas' [] = 0
mediaNotas' lista = (notasAux' lista) / fromIntegral(length lista)

notasAux':: [Alumno'] -> Float
notasAux' [] = 0
notasAux' ((Alumno _ _ nota):xs) = nota + notasAux' xs

-- 4. definir el tipo de datos alumno utilizando sintaxis de registro 
data Alum = Alum {
    dni :: String,
    ne :: Int,
    nota :: Float} deriving Show

aprobado'':: Alum -> Bool
aprobado'' al
    | nota al >=5 = True
    | otherwise = False

calificacionAlumno'' :: Alum -> String
calificacionAlumno'' a
    | nota a < 5 = "Expediente " ++ show (ne a) ++ ", Nota Acta: Suspenso"
    | nota a < 6 = "Expediente " ++ show (ne a) ++ ", Nota Acta: Suficiente"
    | nota a < 7 = "Expediente " ++ show (ne a) ++ ", Nota Acta: Bien"
    | nota a < 9 = "Expediente " ++ show (ne a) ++ ", Nota Acta: Notable"
    | otherwise = "Expediente " ++ show (ne a) ++ ", Nota Acta: Sobresaliente"

dameNota'':: Alum -> Nota
dameNota'' a = nota a   

mediaNotas'':: [Alum] -> Float
mediaNotas'' [] = 0
mediaNotas'' lista = (notasAux'' lista) / fromIntegral(length lista)

notasAux'':: [Alum] -> Float
notasAux'' [] = 0
notasAux'' (a:xs) = nota a + notasAux'' xs

-- 5. dado el siguietne tip ode datos:
data Complejo = Com Float Float deriving Show  
    -- implemente una funcion que devuelva su parte real
parteReal:: Complejo -> Float
parteReal (Com pr _) = pr

    -- una funcion que sume dos complejos
sumaComplejos:: Complejo -> Complejo -> Complejo
sumaComplejos (Com r1 i1) (Com r2 i2)= Com (r1+r2) (i1+i2)


--------------------------------------------------- Lista II ---------------------------------------------------

-- 1. funcion polimorfica que dada una lsita de elementos diga si todos son iguales.
todosIguales:: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs) 

-- 2. Implementar una fuincion polimorfica que dada dos listas diga si la primera es subconjunto de la segunda.
subconjunto:: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = all (`elem` ys) xs

-- 3. Dada la siguiente definicion haz una funcion que sume, una que reste y una que multiplique
data Natural = Cero | Suc Natural deriving Show

sumaNat:: Natural -> Natural -> Natural
sumaNat Cero n = n
sumaNat n Cero = n
sumaNat (Suc n1) (Suc n2) = Suc (sumaNat n1 n2) 

restaNat:: Natural -> Natural -> Natural
restaNat Cero n = Cero
restaNat n Cero = n
restaNat (Suc n1) (Suc n2) = restaNat n1 n2


--------------------------------------------------- Lista III ---------------------------------------------------

-- 1.
data Fecha = Fecha {
    dia :: Int,
    mes :: Int,
    anio :: Int
}

instance Show Fecha where
    show fecha = getDia (fecha) ++ "/" ++ getMes (fecha)

getDia:: Fecha -> String
getDia (Fecha d _ _) = show d

getMes:: Fecha -> String
getMes (Fecha _ m _) = show m

-- 3. 
divisiones :: (Integral a) => a -> [a] -> [Maybe a]
divisiones n lista = map (\x -> if x == 0 then Nothing else Just (n `div` x)) lista