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


-- Operaciones con Personas --

type Billetera = Float
type EventoSobreBilletera =  Billetera -> Persona -> Persona
type EventoSobrePersona = Persona -> Persona

nuevaBilletera :: Billetera -> Persona -> Persona
nuevaBilletera unNumero unaPersona = unaPersona {billetera = unNumero}


-- Eventos --

--Tests sobre eventos
eventosTest = hspec $ do
  describe "Probando los eventos\n" $ do
  it "1. billeteraDe10Creditos sufre deposito" $ depositar 10 billeteraDe10Creditos `shouldBe` nuevaBilletera 20 billeteraDe10Creditos
  it "2. billeteraDe10Creditos sufre extraccion de 3 unidades" $ extraccion 3 billeteraDe10Creditos `shouldBe` nuevaBilletera 7 billeteraDe10Creditos
  it "3. billeteraDe10Creditos sufre extraccion de 15 unidades" $ extraccion 15 billeteraDe10Creditos `shouldBe` nuevaBilletera 0 billeteraDe10Creditos
  it "4. billeteraDe10Creditos sufre un upgrade" $ upgrade billeteraDe10Creditos `shouldBe` nuevaBilletera 12 billeteraDe10Creditos
  it "5. billeteraDe10Creditos sufre cierre de cuenta" $ cierreDeCuenta billeteraDe10Creditos `shouldBe` nuevaBilletera 0 billeteraDe10Creditos
  it "6. billeteraDe10Creditos queda igual" $ quedaIgual billeteraDe10Creditos `shouldBe` nuevaBilletera 10 billeteraDe10Creditos
  it "7.billeteraDe10Creditos sufre deposito de 1000 unidades y luego un upgrade" $  (upgrade.depositar 1000) billeteraDe10Creditos `shouldBe` nuevaBilletera 1020 billeteraDe10Creditos
  describe "\nProbando los eventos en Usuarios\n" $ do
  it "8. Estado de la billetera de pepe" $ billetera pepe `shouldBe` 10
  it "9. Pepe cerro su cuenta" $ cierreDeCuenta pepe `shouldBe` nuevaBilletera 0 pepe
  it "10. Pepe deposita 15 monedas, extrae 2 y obtiene un upgrade" $ (upgrade.extraccion 2.depositar 15) pepe `shouldBe` nuevaBilletera 27.6 pepe


depositar :: EventoSobreBilletera
depositar plata unaPersona = nuevaBilletera (billetera unaPersona + plata) unaPersona

extraccion :: EventoSobreBilletera
extraccion plata unaPersona = nuevaBilletera(max (billetera unaPersona - plata) 0) unaPersona

upgrade :: EventoSobrePersona
upgrade unaPersona = nuevaBilletera (billetera unaPersona + min (billetera unaPersona * 0.2) 10) unaPersona

cierreDeCuenta :: EventoSobrePersona
cierreDeCuenta unaPersona = nuevaBilletera 0 unaPersona

quedaIgual :: EventoSobrePersona
quedaIgual unaPersona = id unaPersona


-- Transacciones --

--Tests transacciones y nuevos eventos
transaccionesTests = hspec $ do
 describe "\nProbando las transacciones\n" $ do
  it "11. Pepe realiza transacción 1" $ luchoCierraLaCuenta pepe `shouldBe` pepe
  it "    transacción 1 sobre billetera de 20 unidades" $ luchoCierraLaCuenta pepe2 `shouldBe` pepe2
  it "12. Pepe realiza transacción 2" $ pepeDeposita5Monedas pepe `shouldBe` nuevaBilletera 15 pepe
  it "13.  Pepe2 realiza transacción 2 con billetera de 50 unidades" $ (pepeDeposita5Monedas. depositar 30) pepe2 `shouldBe` nuevaBilletera 55 pepe2
  it "14. Pepe toca y se va" $ tocoYMeVoy pepe `shouldBe` nuevaBilletera 0 pepe
  it "    Lucho toca y se va" $ luchoTocaYSeVa lucho `shouldBe` nuevaBilletera 0 lucho
  it "15. Pepe es un ahorrante errante" $ ahorranteErrante pepe `shouldBe` nuevaBilletera 34 pepe
  it "    Lucho es un ahorrante errante" $ luchoEsUnAhorranteErrante lucho `shouldBe` nuevaBilletera 24.4 lucho  

type Transaccion = Persona -> Persona
luchoCierraLaCuenta :: Transaccion
pepeDeposita5Monedas :: Transaccion

luchoCierraLaCuenta unaPersona | nombre unaPersona == "Luciano" = cierreDeCuenta unaPersona
                               | otherwise = quedaIgual unaPersona

transaccion1 = luchoCierraLaCuenta

pepeDeposita5Monedas unaPersona | nombre unaPersona == "Jose" = depositar 5 unaPersona
                                | otherwise = quedaIgual unaPersona

transaccion2 = pepeDeposita5Monedas

-- Nuevos Eventos --

tocoYMeVoy = cierreDeCuenta.upgrade.(depositar 15)

ahorranteErrante = (depositar 10).upgrade.(depositar 8).(extraccion 1).(depositar 2).(depositar 1)

luchoTocaYSeVa unaPersona | nombre unaPersona == "Luciano" = tocoYMeVoy unaPersona
                          | otherwise = quedaIgual unaPersona

transaccion3 = luchoTocaYSeVa

luchoEsUnAhorranteErrante unaPersona | nombre unaPersona == "Luciano" = ahorranteErrante unaPersona
                                     | otherwise = quedaIgual unaPersona

transaccion4 = luchoEsUnAhorranteErrante

-- Pago entre usuarios --

--Tests pago entre usuarios y transacciones
pagoEntreUsuariosTests = hspec $ do
 describe "\nProbando el pago entre usuarios\n" $ do
  it "16. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho pepe `shouldBe` nuevaBilletera 3 pepe
  it "17. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho lucho `shouldBe` nuevaBilletera 9 lucho
  it "18. Aplicamos transaccion 1 a Pepe y debe quedar igual" $ transaccion1 pepe `shouldBe` quedaIgual pepe
  it "19. Aplicamos transaccion 5 a Lucho y debe producir que quede con 9 unidades en su billetera" $ transaccion5 lucho `shouldBe` nuevaBilletera 9 lucho
  it "20. Aplicamos transaccion 5 y transaccion 2  a Pepe como resultado debe quedar con 8 unidades en su billetera" $(transaccion2.transaccion5) pepe `shouldBe` nuevaBilletera 8 pepe


usuarioRecibePago :: Float -> Persona -> Persona
usuarioRecibePago unMonto = depositar unMonto

usuarioRealizaPago :: Float -> Persona -> Persona
usuarioRealizaPago unMonto = extraccion unMonto

pepeLeDa7UnidadesALucho :: Transaccion
pepeLeDa7UnidadesALucho unaPersona | nombre unaPersona == "Jose" = usuarioRealizaPago 7 unaPersona
                                   | nombre unaPersona == "Luciano" = usuarioRecibePago 7 unaPersona
                                   | otherwise = quedaIgual unaPersona

transaccion5 = pepeLeDa7UnidadesALucho

-- Bloque -- 
	
type Bloque = [Transaccion]

bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]

-- Test bloques

bloqueTests = hspec $ do
describe "\nProbando el bloques\n" $ do
    it "21. Aplicamos bloque1 a pepe y deberia resultar un nuevo pepe con 18 unidades en su billetera" $ aplicarBloque bloque1 pepe `shouldBe` nuevaBilletera 18 pepe

aplicarBloque :: Bloque -> Persona -> Persona
aplicarBloque (cabeza : cola) = ((aplicarBloque cola).cabeza)
aplicarBloque [] = quedaIgual

saldosMayoresN :: [Persona] -> Bloque -> Float -> [Persona] 
saldosMayoresN [personas] bloque numero = (filter ((>numero).billetera).map(aplicarBloque bloque))[personas]
