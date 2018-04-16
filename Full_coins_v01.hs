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
pepe2 = Persona "Jose" 20
billeteraDe10Creditos = Persona "billeteraDe10Creditos" 10

-- Test
ejecutarTests = hspec $ do
  describe "Probando los eventos\n" $ do
  it "billeteraDe10Creditos sufre deposito" $ depositar 10 billeteraDe10Creditos `shouldBe` nuevaBilletera 20 billeteraDe10Creditos
  it "billeteraDe10Creditos sufre extraccion de 3 unidades" $ extraccion 3 billeteraDe10Creditos `shouldBe` nuevaBilletera 7 billeteraDe10Creditos
  it "billeteraDe10Creditos sufre extraccion de 15 unidades" $ extraccion 15 billeteraDe10Creditos `shouldBe` nuevaBilletera 0 billeteraDe10Creditos
  it "billeteraDe10Creditos sufre un upgrade" $ upgrade billeteraDe10Creditos `shouldBe` nuevaBilletera 12 billeteraDe10Creditos
  it "billeteraDe10Creditos sufre cierre de cuenta" $ cierreDeCuenta billeteraDe10Creditos `shouldBe` nuevaBilletera 0 billeteraDe10Creditos
  it "billeteraDe10Creditos queda igual" $ quedaIgual billeteraDe10Creditos `shouldBe` nuevaBilletera 10 billeteraDe10Creditos
  it "billeteraDe10Creditos sufre deposito de 1000 unidades y luego un upgrade" $  (upgrade.depositar 1000) billeteraDe10Creditos `shouldBe` nuevaBilletera 1020 billeteraDe10Creditos
  describe "\nProbando los eventos en Usuarios\n" $ do
  it "Estado de la billetera de pepe" $ billetera pepe `shouldBe` 10
  it "Pepe cerro su cuenta" $ cierreDeCuenta pepe `shouldBe` nuevaBilletera 0 pepe
  it "Pepe deposita 15 monedas, extrae 2 y obtiene un upgrade" $ (upgrade.extraccion 2.depositar 15) pepe `shouldBe` nuevaBilletera 27.6 pepe
  describe "\nProbando las transacciones\n" $ do
  it "Pepe realiza transacción 1" $ luchoCierraLaCuenta pepe `shouldBe` pepe
  it "Pepe2 realiza transacción 1" $ luchoCierraLaCuenta pepe2 `shouldBe` pepe2
  it "Pepe realiza transacción 2" $ pepeDeposita5Monedas pepe `shouldBe` nuevaBilletera 15 pepe
  it "Pepe2 realiza transacción 2 con billetera de 50 unidades" $ (pepeDeposita5Monedas. depositar 30) pepe2 `shouldBe` nuevaBilletera 55 pepe2
  it "Lucho toca y se va" $ luchoTocaYSeVa lucho `shouldBe` nuevaBilletera 0 lucho
  it "Lucho es un ahorrante errante" $ luchoEsUnAhorranteErrante lucho `shouldBe` nuevaBilletera 24.400002 lucho
  it "Pepe toca y se va" $ tocoYMeVoy pepe `shouldBe` nuevaBilletera 0 pepe
  it "Pepe es un ahorrante errante" $ ahorranteErrante pepe `shouldBe` nuevaBilletera 34 pepe
  describe "\nProbando el pago entre usuarios\n" $ do
  it "Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho pepe `shouldBe` nuevaBilletera 3 pepe
  it "Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho lucho `shouldBe` nuevaBilletera 9 lucho
  
  
-- Operaciones con Personas --
	
nuevaBilletera unNumero unaPersona = unaPersona {billetera = unNumero}


-- Eventos --

depositar plata unaPersona = nuevaBilletera (billetera unaPersona + plata) unaPersona

extraccion plata unaPersona | billetera unaPersona - plata < 0 = nuevaBilletera 0 unaPersona
                            | otherwise = nuevaBilletera (billetera unaPersona - plata) unaPersona

upgrade unaPersona | billetera unaPersona * 0.2 > 10 = nuevaBilletera (billetera unaPersona + 10) unaPersona
                   | otherwise = nuevaBilletera (billetera unaPersona * 1.2) unaPersona

cierreDeCuenta unaPersona = nuevaBilletera 0 unaPersona

quedaIgual unaPersona = nuevaBilletera (billetera unaPersona) unaPersona

-- Transacciones --

type Transaccion = Persona -> Persona
luchoCierraLaCuenta :: Transaccion
pepeDeposita5Monedas :: Transaccion

luchoCierraLaCuenta unaPersona | nombre unaPersona == "Luciano" = cierreDeCuenta unaPersona
                               | otherwise = quedaIgual unaPersona

pepeDeposita5Monedas unaPersona | nombre unaPersona == "Jose" = depositar 5 unaPersona
                                | otherwise = quedaIgual unaPersona

-- Nuevos Eventos --

tocoYMeVoy = cierreDeCuenta.upgrade.(depositar 15)

ahorranteErrante = (depositar 10).upgrade.(depositar 8).(extraccion 1).(depositar 2).(depositar 1)

luchoTocaYSeVa unaPersona | nombre unaPersona == "Luciano" = tocoYMeVoy unaPersona
                          | otherwise = quedaIgual unaPersona

luchoEsUnAhorranteErrante unaPersona | nombre unaPersona == "Luciano" = ahorranteErrante unaPersona
                                     | otherwise = quedaIgual unaPersona

-- Pago entre usuarios --

pepeLeDa7UnidadesALucho :: Transaccion
pepeLeDa7UnidadesALucho unaPersona | nombre unaPersona == "Jose" = extraccion 7 unaPersona
                                   | nombre unaPersona == "Luciano" = depositar 7 unaPersona
                                   | otherwise = quedaIgual unaPersona
