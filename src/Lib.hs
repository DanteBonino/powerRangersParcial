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
calcularNivelDePelea = (sum . map length . habilidades) --También podría hacer foldr ((+) . length) 0 y no hacer sum . map

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
rangerLider unGrupoDeRangers = findOrElse ((== "rojo") . color) (head unGrupoDeRangers) unGrupoDeRangers

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
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = ((>= 5). length . superHabilidades)

--Punto 7:
