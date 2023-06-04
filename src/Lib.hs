module Lib () where

--Punto 1:
--a
data Persona = Persona{
    habilidades :: [String],
    esBuena     :: Bool
}

dante :: Persona
dante = Persona ["falta de atencion"] True

--b
data PowerRanger = PowerRanger{
    color            :: String,
    superHabilidades :: [String],
    nivelDePelea     :: Int
}
powerRangerRojo :: PowerRanger
powerRangerRojo = PowerRanger "rojo" ["superBailar"] 400

--Punto 2:
type Color = String

convertirEnPowerRanger :: Color -> Persona -> PowerRanger
convertirEnPowerRanger unColor unaPersona = PowerRanger unColor ((potenciarHabilidades . habilidades) unaPersona) (calcularNivelDePelea unaPersona)

potenciarHabilidades :: [String] -> [String]
potenciarHabilidades = map (("super" ++) . ponerEnMayusculaLaPrimerLetra)

ponerEnMayusculaLaPrimerLetra :: String -> String
ponerEnMayusculaLaPrimerLetra (primerLetra : restoDeLetras) = toUppercase primerLetra ++ restoDeLetras 

calcularNivelDePelea :: Persona -> Int
calcularNivelDePelea = (cantidadDeLetrasDeTodasLasPalabras . habilidades) --También podría hacer foldr ((+) . length) 0 y no hacer sum . map

cantidadDeLetrasDeTodasLasPalabras :: [String] -> Int
cantidadDeLetrasDeTodasLasPalabras = sum . map length
--Punto 3:
formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger unosColores []  = []
formarEquipoRanger [] unasPersonas = []
formarEquipoRanger (unColor : restoDeColores) (unaPersona : restoDePersonas)
    | esBuena unaPersona = convertirEnPowerRanger unColor unaPersona : formarEquipoRanger restoDeColores restoDePersonas
    | otherwise          = formarEquipoRanger  (unColor : restoDeColores) restoDePersonas

--Punto 4:
--a
findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse unaCondicion unValor unosValores
    | any unaCondicion unosValores = (head . filter unaCondicion) unosValores
    | otherwise                    = unValor
--b
rangerLider :: [PowerRanger] -> PowerRanger
rangerLider  = lider color

--Punto 5:
--a
maximumBy :: (Ord a) => (b -> a) -> [b] -> b
maximumBy transformador = foldl1 (maximoSegun transformador)

maximoSegun ::(Ord a) => (b -> a) -> b -> b -> b
maximoSegun transformador unValor otroValor
    | transformador unValor > transformador otroValor = unValor
    | otherwise                                       = otroValor
--b
rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso = maximumBy nivelDePelea

--Punto 6:
esRangerHabilidoso :: PowerRanger -> Bool
esRangerHabilidoso = ((>= 5). length . superHabilidades)

--Punto 7:
alfa5 :: PowerRanger
alfa5 = PowerRanger "metalico" ["reparar cosas", cycle "ay"] 100

--b
--Función que terminaría:
-- rangerHabilidoso alfa5, ya que, si bien una habilidad es infinitamente larga, eso a dicha función no le importa pq lo que considera es la cantidad de elementos que tiene la lista de superHabilidadades y nó la longitud de los subElementos
--Función que no terminaría:
-- maximumBy sumaDeLaLongitudDeHabilidades [powerRangerRojo, alfa5] -> Acá no terminaría más pq no podría calcular la sumaDeLaLongitudDeHabilidades de alfa5, dado que length para terminar de ejecutarse debe recorrer toda la lista y si se hace length de algo infinito, loopea
-- donde sumaDeLongitudDeHabilidades = (cantidadDeLetrasDeTodasLasPalabras . superHabilidades)

--Punto 8:
data ChicaPoderosa = ChicaPoderosa{
    cantidadDePelo :: Int,
    colorDeChica          :: String
}

chicaLider :: [ChicaPoderosa] -> ChicaPoderosa
chicaLider = lider colorDeChica

lider :: (a->String)-> [a] -> a
lider f listaDeValores = findOrElse (esRojo . f) (head listaDeValores) listaDeValores

esRojo :: String -> Bool
esRojo = (== "rojo")
