{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Persona = Persona {
    nombre :: String,
    billetera :: Billetera
} deriving (Show, Eq)


pepe = Persona "Jose" 10
lucho = Persona "Luciano" 2
pepe2 = Persona "Jose" 20

type Billetera = Float
type Plata = Float
type Evento = Billetera -> Billetera

nuevaBilletera :: Billetera -> Persona -> Persona
nuevaBilletera unNumero unaPersona = unaPersona {billetera = unNumero}

testEventosBilleteras = hspec $ do
  describe "Probando los eventos\n" $ do
  it "1. A una billetera de 10 creditos le depositamos 10 monedas" $ depositar 10 10 `shouldBe` 20
  it "2. A una billetera de 10 creditos le extraemos 3 monedas" $ extraccion 3 10 `shouldBe` 7
  it "3. A una billetera de 10 creditos le extraemos 15 monedas" $ extraccion 15 10 `shouldBe` 0
  it "4. A una billetera de 10 creditos le hacemos un upgrade" $ upgrade 10 `shouldBe` 12
  it "5. Una billetera de 10 creditos sufre un cierre de cuenta" $ cierreDeCuenta 10 `shouldBe` 0
  it "6. A una billetera de 10 creditos le aplicamos queda igual" $ quedaIgual 10 `shouldBe` 10
  it "7. A una billetera de 10 creditos le depositamos 1000 monedas y luego un upgrade" $  (upgrade.depositar 1000) 10 `shouldBe` 1020
  describe "\nProbando los eventos en Usuarios\n" $ do
  it "8. Estado de la billetera de pepe" $ billetera pepe `shouldBe` 10
  it "9. Pepe cerro su cuenta" $ (cierreDeCuenta.billetera) pepe `shouldBe` 0
  it "10. Pepe deposita 15 monedas, extrae 2 y obtiene un upgrade" $ (upgrade.extraccion 2.depositar 15.billetera) pepe `shouldBe` 27.6

-- Eventos --

depositar :: Plata -> Evento
depositar = (+)

extraccion :: Plata -> Evento
extraccion plata unaBilletera = max 0 (unaBilletera - plata)

upgrade :: Evento
upgrade unaBilletera = (((+) unaBilletera).(min 10).((*) 0.2)) unaBilletera

cierreDeCuenta :: Evento
cierreDeCuenta _ = 0

quedaIgual :: Evento
quedaIgual = id

--Tests transacciones y nuevos eventos

testTransacciones = hspec $ do
 describe "\nProbando las transacciones\n" $ do
  it "11. Pepe realiza transacción 1" $ luchoCierraLaCuenta pepe `shouldBe` 10
  it "    Transacción 1 sobre billetera de 20 unidades" $ luchoCierraLaCuenta pepe2 `shouldBe` 20
  it "12. Pepe realiza transacción 2" $ pepeDeposita5Monedas pepe `shouldBe` 15
  it "13. Pepe2 realiza transacción 2 con billetera de 50 unidades" $ ((depositar 30).pepeDeposita5Monedas) pepe2 `shouldBe` 55
  it "14. Pepe toca y se va" $ (tocoYMeVoy.billetera) pepe `shouldBe` 0
  it "    Lucho toca y se va" $ luchoTocaYSeVa lucho `shouldBe` 0
  it "15. Pepe es un ahorrante errante" $ (ahorranteErrante.billetera) pepe `shouldBe` 34
  it "    Lucho es un ahorrante errante" $ luchoEsUnAhorranteErrante lucho `shouldBe` 24.4
 describe "\nProbando el pago entre usuarios\n" $ do
  it "16. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho pepe `shouldBe` 3
  it "17. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho lucho `shouldBe` 9
  it "18. Aplicamos transaccion 1 a Pepe y debe quedar igual" $ impactarTransaccion transaccion1 pepe `shouldBe` pepe
  it "19. Aplicamos transaccion 5 a Lucho y debe producir que quede con 9 unidades en su billetera" $ impactarTransaccion transaccion5 lucho `shouldBe` nuevaBilletera 9 lucho
  it "20. Aplicamos transaccion 5 y transaccion 2  a Pepe como resultado debe quedar con 8 unidades en su billetera" $ ((impactarTransaccion transaccion2).(impactarTransaccion transaccion5)) pepe`shouldBe` nuevaBilletera 8 pepe


  --it "20. Aplicamos transaccion 5 y transaccion 2  a Pepe como resultado debe quedar con 8 unidades en su billetera" $ nuevaBilletera(transaccion2.(flip (nuevaBilletera) pepe).transaccion5) pepe `shouldBe` nuevaBilletera 8 pepe

type Transaccion = Persona -> Billetera

generarTransacciónSimple unEvento unNombre unaPersona | nombre unaPersona == nombre unNombre = (unEvento.billetera) unaPersona
                                                      | otherwise = (quedaIgual.billetera) unaPersona

luchoCierraLaCuenta :: Transaccion
luchoCierraLaCuenta = generarTransacciónSimple cierreDeCuenta lucho

transaccion1 :: Transaccion
transaccion1 = luchoCierraLaCuenta

pepeDeposita5Monedas :: Transaccion
pepeDeposita5Monedas = generarTransacciónSimple (depositar 5) pepe

transaccion2 :: Transaccion
transaccion2 = pepeDeposita5Monedas

-- Nuevos Eventos --

tocoYMeVoy = cierreDeCuenta.upgrade.(depositar 15)

ahorranteErrante = (depositar 10).upgrade.(depositar 8).(extraccion 1).(depositar 2).(depositar 1)

luchoTocaYSeVa :: Transaccion
luchoTocaYSeVa = generarTransacciónSimple tocoYMeVoy lucho

transaccion3 :: Transaccion
transaccion3 = luchoTocaYSeVa

luchoEsUnAhorranteErrante :: Transaccion
luchoEsUnAhorranteErrante = generarTransacciónSimple ahorranteErrante lucho

transaccion4 :: Transaccion
transaccion4 = luchoEsUnAhorranteErrante

-- Pago entre usuarios --

generarTransacciónCompleja unEventoUno unEventoDos unNombreUno unNombreDos unaPersona | nombre unaPersona == nombre unNombreUno = (unEventoUno.billetera) unaPersona
                                                                                      | nombre unaPersona == nombre unNombreDos = (unEventoDos.billetera) unaPersona
                                                                                      | otherwise = (quedaIgual.billetera) unaPersona

type TransaccionEntreUsuarios = Persona -> Billetera

pepeLeDa7UnidadesALucho :: TransaccionEntreUsuarios
pepeLeDa7UnidadesALucho = generarTransacciónCompleja (extraccion 7) (depositar 7) pepe lucho

transaccion5 :: Transaccion
transaccion5 = pepeLeDa7UnidadesALucho


impactarTransaccion :: Transaccion -> Persona -> Persona
impactarTransaccion unaTransaccion unaPersona = nuevaBilletera (unaTransaccion unaPersona) unaPersona
impactarTransaccion2 unaPersona unaTransaccion = nuevaBilletera (unaTransaccion unaPersona) unaPersona

-- Bloque --

type Bloque = [Transaccion]

bloque1 :: Bloque
bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]

bloque2 :: Bloque
bloque2 = [transaccion2,transaccion2,transaccion2,transaccion2,transaccion2]

type Usuarios = [Persona]
usuarios = [pepe, lucho]

transaccionAUsuario = map (impactarTransaccion)

aplicarBloque :: Bloque -> Persona -> Persona
aplicarBloque unBloque unaPersona= foldr ($) unaPersona (transaccionAUsuario unBloque)

usuariosConSaldoMayorANumero :: Billetera -> Bloque -> Usuarios -> Usuarios
usuariosConSaldoMayorANumero unNumero unBloque = filter ((> unNumero).billetera.aplicarBloque unBloque)
