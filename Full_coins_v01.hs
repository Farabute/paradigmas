{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec


data Persona = Persona {
    nombre :: String,
    billetera :: Float
} deriving (Show, Eq)

pepe = Persona "Jose" 10
lucho = Persona "Luciano" 2

nuevaBilletera unNumero unaBilletera = unaBilletera {billetera = unNumero}

-- Eventos --

depositar plata usuario = nuevaBilletera (billetera usuario + plata) usuario

extraccion plata usuario | billetera usuario - plata < 0 = nuevaBilletera 0 usuario
                         | otherwise = nuevaBilletera (billetera usuario - plata) usuario

upgrade usuario | billetera usuario * 0.2 > 10 = nuevaBilletera (billetera usuario + 10) usuario
                | otherwise = nuevaBilletera (billetera usuario * 1.2) usuario

cierreDeCuenta usuario = nuevaBilletera 0 usuario

quedaIgual usuario = nuevaBilletera usuario
