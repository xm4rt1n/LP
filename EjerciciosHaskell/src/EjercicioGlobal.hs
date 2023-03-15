module EjercicioGlobal where
import System.IO
import System.IO.Unsafe

type Nombre = String
type Ciudad = String
type Pais = String
type Ocupacion = String

data Personaje = P Nombre Ciudad Pais Ocupacion String deriving Show

-- Recibe la linea y el delimitador
partirLinea :: String -> Char -> [String]
partirLinea linea d = partirLineaAux linea d [] []

partirLineaAux :: String -> Char -> String -> [String] -> [String]
partirLineaAux [] _ previous ac = ac
partirLineaAux (x:xs) d palabra ac = if (x==d) then partirLineaAux xs d [] ac ++ [palabra] else partirLineaAux xs d (palabra++[x]) ac

leerEntrada:: IO [String]
leerEntrada = do
    contenido <- readFile "database.csv"
    let todoTasks = lines contenido
    return todoTasks

lineas:: [String]
lineas = unsafePerformIO (leerEntrada)
main = do
    handle <- openFile "database.csv" ReadMode   
    contents <- hGetContents handle
    let todoTasks = lines contents
    print(todoTasks)    
   
    hClose handle


-- Prueba
p:: [a] -> [[a]]
p [] = [[]]
p (x:xs) = []: map (x:) (p xs)
