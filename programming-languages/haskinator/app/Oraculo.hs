-- Modulo Oraculo.hs: Implementa el tipo de datos Oraculo
--   y funciones para manipularlo
--
-- Autores:
--   Cristian Medina 10-10445
--   Manuel Pacheco 10-10524

module Oraculo
( Oraculo
, crearPrediccion
, crearPregunta
, prediccion
, pregunta
, positivo
, negativo
, obtenerCadena
, obtenerEstadisticas
) where

showsOraculo  :: Oraculo -> String -> String
showsOraculo (Prediccion s) x = '@': (shows s x)
showsOraculo (Pregunta q t f) x = '{':( shows q (':' : ( showsOraculo t ('-' : showsOraculo f ('}' : x)))))


readsOraculo [] = []

readsOraculo ('{':s)  =  [(Pregunta q t f, w) | (q, ':':u) <- reads s,
                                  (t, '-':v) <- readsOraculo u,
                                  (f, '}':w) <- readsOraculo v]

readsOraculo ('@':s)  =  [(Prediccion p, t)     | (p,t)      <- reads s]

data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo
    --deriving (Read)
instance Show Oraculo
    where   showsPrec _ x = showsOraculo x

instance Read Oraculo
    where readsPrec _ x = readsOraculo x

-- Constructores

crearPrediccion :: String -> Oraculo
crearPrediccion x = Prediccion x

crearPregunta :: String -> Oraculo -> Oraculo -> Oraculo
crearPregunta s t f = Pregunta s t f

-- Acceso

prediccion :: Oraculo -> String
prediccion (Prediccion pStr) = pStr
prediccion (Pregunta _ _ _) = error "El oraculo no es una prediccion"

pregunta :: Oraculo -> String
pregunta (Prediccion _) = error "El oraculo no es una pregunta"
pregunta (Pregunta pStr _ _) = pStr


positivo :: Oraculo -> Oraculo
positivo (Prediccion _) = error "El oraculo no es una pregunta"
positivo (Pregunta _ pos _) = pos

negativo :: Oraculo -> Oraculo
negativo (Prediccion _) = error "El oraculo no es una pregunta"
negativo (Pregunta _ _ neg) = neg

-- Modificadores

obtenerCadena :: Oraculo -> String -> Maybe [(String, Bool)]
obtenerCadena (Prediccion p) s = if p == s then Just [] else Nothing
obtenerCadena (Pregunta p pos neg) s
  | subCadPos /= Nothing = agregarMaybeLista (p, True) subCadPos
  | subCadNeg /= Nothing = agregarMaybeLista (p, False) subCadNeg
  | otherwise            = Nothing
  where
    agregarMaybeLista a (Just b) = Just (a:b)
    subCadPos = obtenerCadena pos s
    subCadNeg = obtenerCadena neg s


obtenerMax :: Oraculo -> Integer
obtenerMax (Prediccion _) = 0
obtenerMax (Pregunta _ r l ) =
  if (obtenerMax r > obtenerMax l) then
    obtenerMax r + 1
  else
    obtenerMax l +1

obtenerMin :: Oraculo -> Integer
obtenerMin (Prediccion _) = 0
obtenerMin (Pregunta _ r l ) =
  if (obtenerMin r > obtenerMin l) then
    obtenerMin l + 1
  else
    obtenerMin r +1

obtenerNiveles :: Oraculo -> ([Integer],Integer)
obtenerNiveles (Prediccion _) = ([0], 1)
obtenerNiveles (Pregunta _ t f) = ( map fmas nivelesT ++ nivelesF , numF + numT )
  where
    fmas = (\x -> x+1)
    nivelesF = fst (obtenerNiveles f)
    nivelesT = fst (obtenerNiveles t)
    numF = snd (obtenerNiveles f)
    numT = snd (obtenerNiveles t)
    unionNiveles = (\x y -> x + y )

obtenerPromedio :: Oraculo -> Rational
obtenerPromedio o = toRational bleh / (toRational (snd(ln)))
    where
      bleh = foldr (+) 0 ( fst(ln) )
      ln = obtenerNiveles o

obtenerEstadisticas :: Oraculo -> (Integer, Integer, Rational)
obtenerEstadisticas o = (obtenerMin o, obtenerMax o, obtenerPromedio o)
