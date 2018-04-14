{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- Test

ejecutarTests = hspec $ do
  describe "Probando los eventos\n" $ do
    it "Pepe deposita 15 monedas" (depositar 15 pepe `shouldBe` nuevaBilletera 25 pepe)
    it "Lucho extrae 5 monedas" (extraccion 5 lucho `shouldBe` nuevaBilletera 0 lucho)
    it "Pepe recibe un upgrade" (upgrade pepe `shouldBe` nuevaBilletera 12 pepe)
    it "Pepe cerro su cuenta" (cierreDeCuenta pepe `shouldBe` nuevaBilletera 0 pepe)
    it "Lucho no cambio su billetera" (quedaIgual lucho `shouldBe` nuevaBilletera 2 lucho)
    it "Pepe toca y se va" (tocoYMeVoy pepe `shouldBe` nuevaBilletera 0 pepe)
    it "Pepe es un ahorrante errante" (ahorranteErrante pepe `shouldBe` nuevaBilletera 34 pepe)
  describe "\nProbando las transacciones\n" $ do
    it "Lucho cierra la cuenta" (luchoCierraLaCuenta lucho `shouldBe` nuevaBilletera 0 lucho)
    it "Pepe deposita 5 monedas" (pepeDeposita5Monedas pepe `shouldBe` nuevaBilletera 15 pepe)
    it "Lucho toca y se va" (luchoTocaYSeVa lucho `shouldBe` nuevaBilletera 0 lucho)
    it "Lucho es un ahorrante errante" (luchoEsUnAhorranteErrante lucho `shouldBe` nuevaBilletera 24.400002 lucho)
  describe "\nProbando el pago entre usuarios\n" $ do
    it "Pepe le da 7 unidades a Lucho" (pepeLeDa7UnidadesALucho pepe `shouldBe` nuevaBilletera 3 pepe)
    it "Pepe le da 7 unidades a Lucho" (pepeLeDa7UnidadesALucho lucho `shouldBe` nuevaBilletera 9 lucho)

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

pepeLeDa7UnidadesALucho :: Transaccion
pepeLeDa7UnidadesALucho unaPersona | nombre unaPersona == "Jose" = extraccion 7 unaPersona
                                   | nombre unaPersona == "Luciano" = depositar 7 unaPersona
                                   | otherwise = quedaIgual unaPersona
