-- Modulo Haskinator.hs: Implementa el cliente por consola
--   de las funcionalidades ofrecidas por Oraculo.hs
--
-- Autores:
--   Cristian Medina 10-10445
--   Manuel Pacheco 10-10524

import Oraculo
import System.IO
import Control.Exception as E

-- Funciones principales

main :: IO ()
main = do
  putStrLn showStartup
  -- #! mainHelper (Just (crearPregunta "TIENE VERRUGA?" (crearPrediccion "ES CHAVEZ") (crearPrediccion "ES MADURO")))
  mainHelper Nothing

mainHelper :: Maybe Oraculo -> IO ()
mainHelper oraculo = do
  putStrLn showMenu
  option <- getLine
  putStrLn ""
  case option of
    "a" -> opcionCrearOraculo oraculo
    "b" -> opcionPredecir oraculo
    "c" -> opcionPersistir oraculo
    "d" -> opcionCargar oraculo
    "e" -> opcionPreguntaCrucial oraculo
    "f" -> opcionConsultarEstadisticas oraculo
    "g" -> do
      putStrLn "Aburrido... Nos veremos luego :/\n"
      return ()
    _   -> do
      putStrLn "Opcion Inválida\n"
      mainHelper oraculo

-- Funciones de texto

showStartup :: String
showStartup = "------------------------\n\
              \Bienvenidos a Haskinator\n\
              \------------------------\n"

showMenu :: String
showMenu = "Elige una de las siguientes opciones (por su letra):\n\
           \    a. Crear un Oráculo Nuevo.\n\
           \    b. Predecir.\n\
           \    c. Persistir.\n\
           \    d. Cargar.\n\
           \    e. Consultar pregunta crucial.\n\
           \    f. Consultar estadísticas.\n\
           \    g. Cerrar el programa.\n"

-- Funciones de opciones

opcionCrearOraculo :: Maybe Oraculo -> IO ()
opcionCrearOraculo oraculo = do
  putStrLn "Se ha creado un oraculo nuevo.\n"
  mainHelper Nothing

opcionPredecir :: Maybe Oraculo -> IO ()
opcionPredecir (Nothing) = do
  putStrLn "El oráculo es nuevo (Está vacío).\n"
  mainHelper (Nothing)
opcionPredecir (Just nodo) = do
  navegarGeneral nodo
  mainHelper (Just nodo)

opcionPreguntaCrucial :: Maybe Oraculo -> IO ()
opcionPreguntaCrucial (Nothing) = do
  putStrLn "El oráculo es nuevo (Está vacío).\n"
  mainHelper (Nothing)
opcionPreguntaCrucial (Just oraculo) = do
  putStrLn "Prediccion A:"
  a <- getLine
  putStrLn "Prediccion B:"
  b <- getLine
  putStrLn ("La pregunta crucial es:" ++ (obtenerCrucial oraculo a b))
  mainHelper (Just oraculo)

opcionConsultarEstadisticas :: Maybe Oraculo -> IO ()
opcionConsultarEstadisticas Nothing = do
  putStrLn "La consulta es inválida (Oráculo Vacío).\n"
  mainHelper Nothing
opcionConsultarEstadisticas (Just oraculo) = do
  let (x,y,z) = obtenerEstadisticas oraculo
  putStrLn ("El mínimo de preguntas es: " ++ show x)
  putStrLn ("El máximo de preguntas es: " ++ show y)
  putStrLn ("El promedio de preguntas es: " ++ show z ++ "\n")
  mainHelper (Just oraculo)

opcionPersistir :: Maybe Oraculo -> IO()
opcionPersistir (Nothing) = do
  putStrLn "El oraculo no existe.\n"
  mainHelper (Nothing);
opcionPersistir (Just x) = do
  putStrLn "Escriba el nombre del archivo donde guardar el Oraculo:"
  fileName <- getLine
  let content = (show x)
  writeFile fileName content
  mainHelper (Just x)


opcionCargar :: Maybe Oraculo -> IO()
opcionCargar x = do
  putStrLn "Escriba el nombre del archivo de donde cargar el Oraculo:"
  fileName <- getLine
  fileStream <- openFile fileName ReadMode
  content  <- hGetContents fileStream
  let z = (read content) :: Oraculo
  mainHelper (Just z)


-- Helpers de opcionPredecir

navegarGeneral :: Oraculo -> IO ()
navegarGeneral oraculo =
  E.catch (navegarPrediccion oraculo) (navegarPregunta oraculo)

navegarPrediccion :: Oraculo -> IO ()
navegarPrediccion oraculo = putStrLn (prediccion oraculo ++ "\n")

navegarPregunta :: Oraculo -> SomeException -> IO ()
navegarPregunta oraculo error = do
  putStrLn (pregunta oraculo ++ "\n")
  respuesta <- getLine
  putStrLn ""
  if (respuesta == "si")
    then navegarGeneral (positivo oraculo)
    else navegarGeneral (negativo oraculo)

-- Helpers de opcionPreguntaCrucial

obtenerCrucial :: Oraculo -> String -> String -> String
obtenerCrucial oraculo a b = fst . fst . head . (dropWhile iguales) $ zip (cadL $ obtenerCadena oraculo a) (cadL $ obtenerCadena oraculo a)
  where
    cadL Nothing = []
    cadL (Just x) = x
    iguales = \((x1,y1),(x2,y2)) -> if (y1 == y2) then True else False
