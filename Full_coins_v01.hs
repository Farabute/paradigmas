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
pepe2 = Persona "Jose" 20
lucho = Persona "Luciano" 2

nuevaBilletera unNumero unaPersona = unaPersona {billetera = unNumero}

-- Eventos --

depositar plata unaPersona = nuevaBilletera (billetera unaPersona + plata) unaPersona

extraccion plata unaPersona | billetera unaPersona - plata < 0 = nuevaBilletera 0 unaPersona
                            | otherwise = nuevaBilletera (billetera unaPersona - plata) unaPersona

upgrade unaPersona | billetera unaPersona * 0.2 > 10 = nuevaBilletera (billetera unaPersona + 10) unaPersona
                   | otherwise = nuevaBilletera (billetera unaPersona * 1.2) unaPersona

cierreDeCuenta unaPersona = nuevaBilletera 0 unaPersona

quedaIgual unaPersona = nuevaBilletera (billetera unaPersona) unaPersona

-- Transacciones

type Transaccion = Persona -> Persona
luchoCierraLaCuenta :: Transaccion
pepeDeposita5Monedas :: Transaccion

luchoCierraLaCuenta unaPersona | nombre unaPersona == "Luciano" = cierreDeCuenta unaPersona
                               | otherwise = quedaIgual unaPersona

pepeDeposita5Monedas unaPersona | nombre unaPersona == "Jose" = depositar 5 unaPersona
                                | otherwise = quedaIgual unaPersona

-- Nuevos Eventos

tocoYMeVoy = cierreDeCuenta.upgrade.(depositar 15)

ahorranteErrante = (depositar 10).upgrade.(depositar 8).(extraccion 1).(depositar 2).(depositar 1)

luchoTocaYSeVa unaPersona | nombre unaPersona == "Luciano" = tocoYMeVoy unaPersona
                          | otherwise = quedaIgual unaPersona

luchoEsUnAhorranteErrante unaPersona | nombre unaPersona == "Luciano" = ahorranteErrante unaPersona
                                     | otherwise = quedaIgual unaPersona

-- Pago entre usuarios

pepeLeDa7UnidadesALucho unaPersona | nombre unaPersona == "Jose" = extraccion 7 unaPersona
                                   | nombre unaPersona == "Luciano" = depositar 7 unaPersona
                                   | otherwise = quedaIgual unaPersona
