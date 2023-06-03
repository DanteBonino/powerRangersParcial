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
