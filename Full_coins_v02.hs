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
  it "1. A una billetera de 10 créditos le depositamos 10 monedas" $ depositar 10 10 `shouldBe` 20
  it "2. A una billetera de 10 créditos le extraemos 3 monedas" $ extraccion 3 10 `shouldBe` 7
  it "3. A una billetera de 10 créditos le extraemos 15 monedas" $ extraccion 15 10 `shouldBe` 0
  it "4. A una billetera de 10 créditos le hacemos un upgrade" $ upgrade 10 `shouldBe` 12
  it "5. Una billetera de 10 créditos sufre un cierre de cuenta" $ cierreDeCuenta 10 `shouldBe` 0
  it "6. A una billetera de 10 créditos le aplicamos queda igual" $ quedaIgual 10 `shouldBe` 10
  it "7. A una billetera de 10 créditos le depositamos 1000 monedas y luego un upgrade" $  (upgrade.depositar 1000) 10 `shouldBe` 1020
  describe "\nProbando los eventos en Usuarios\n" $ do
  it "8. Estado de la billetera de pepe" $ billetera pepe `shouldBe` 10
  it "9. Pepe cerró su cuenta" $ (cierreDeCuenta.billetera) pepe `shouldBe` 0
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
  it "11. Pepe realiza transacción 1 sobre billetera de 20 unidades" $ luchoCierraLaCuenta pepe 20 `shouldBe` 20
  it "12. Pepe realiza transacción 2" $ pepeDeposita5Monedas pepe 10 `shouldBe` 15
  it "13. Pepe2 realiza transacción 2 con billetera de 50 unidades" $ pepeDeposita5Monedas pepe2 50 `shouldBe` 55
  it "14. Lucho toca y se va" $ luchoTocaYSeVa lucho 10 `shouldBe` 0
  it "    Pepe toca y se va" $ luchoTocaYSeVa pepe 10 `shouldBe` 10
  it "15. Lucho es un ahorrante errante" $ luchoEsUnAhorranteErrante lucho 10 `shouldBe` 34
  it "    Pepe es un ahorrante errante" $ luchoEsUnAhorranteErrante pepe 10 `shouldBe` 10
 describe "\nProbando el pago entre usuarios\n" $ do
  it "16. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho pepe 10 `shouldBe` 3
  it "17. Pepe le da 7 unidades a Lucho" $ pepeLeDa7UnidadesALucho lucho 10 `shouldBe` 17
  it "18. Aplicamos transacción 1 a Pepe y debe quedar igual" $ impactarTransaccion transaccion1 pepe `shouldBe` pepe
  it "19. Aplicamos transacción 5 a Lucho y debe producir que quede con 9 unidades en su billetera" $ impactarTransaccion transaccion5 lucho `shouldBe` nuevaBilletera 9 lucho
  it "20. Aplicamos transacción 5 y transacción 2 a Pepe como resultado debe quedar con 8 unidades en su billetera" $ ((impactarTransaccion transaccion2).(impactarTransaccion transaccion5)) pepe`shouldBe` nuevaBilletera 8 pepe

type Transaccion = Persona -> Evento

generarTransacciónSimple unEvento unNombre unaPersona | nombre unaPersona == nombre unNombre = unEvento
                                                      | otherwise = quedaIgual

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

generarTransacciónCompleja unNombreUno unNombreDos unMonto unaPersona | nombre unaPersona == nombre unNombreUno = extraccion unMonto
                                                                      | nombre unaPersona == nombre unNombreDos = depositar unMonto
                                                                      | otherwise = quedaIgual

type TransaccionEntreUsuarios = Persona -> Evento

pepeLeDa7UnidadesALucho :: TransaccionEntreUsuarios
pepeLeDa7UnidadesALucho = generarTransacciónCompleja pepe lucho 7

transaccion5 :: Transaccion
transaccion5 = pepeLeDa7UnidadesALucho

impactarTransaccion :: Transaccion -> Persona -> Persona
impactarTransaccion unaTransaccion unaPersona = nuevaBilletera (unaTransaccion unaPersona (billetera unaPersona)) unaPersona
impactarTransaccion2 unaPersona unaTransaccion = nuevaBilletera (unaTransaccion unaPersona (billetera unaPersona)) unaPersona

-- Bloque --

-- Test bloques

testBloques = hspec $ do
  describe "\nProbando los bloques\n" $ do
    it "21. Aplicamos bloque1 a Pepe y debería resultar un nuevo Pepe con 18 unidades en su billetera" $ aplicarBloque bloque1 pepe `shouldBe` nuevaBilletera 18 pepe
    it "22. Aplicamos bloque1 a los usuarios Pepe y Lucho, deberíamos chequear que Pepe es el único con un saldo mayor a 10 en su billetera" $ usuariosConSaldoMayorANumero 10 bloque1 usuarios `shouldBe` [nuevaBilletera 10 pepe]
    it "23. Aplicamos bloque1 a los usuarios Pepe y Lucho, deberíamos chequear que Pepe es el más adinerado" $ usuarioMasAdinerado usuarios bloque1 `shouldBe` pepe
    it "24. Aplicamos bloque1 a los usuarios Pepe y Lucho, deberíamos chequear que Lucho es el menos adinerado" $ usuarioMenosAdinerado usuarios bloque1 `shouldBe` lucho
    it "25. Aplicamos nuestro Block Chain a Pepe, deberíamos chequear que el bloque1 lo deja con peor saldo, y lo deja con una billetera de 18" $ peorBloque blockChain pepe `shouldBe` nuevaBilletera 18 pepe
    it "26. Aplicamos nuestro Block Chain a Pepe, debería quedar con una billetera de 115" $ aplicarBlockChainAUsuario blockChain pepe `shouldBe` nuevaBilletera 115 pepe
    it "27. Aplicamos los primeras 3 bloques de nuestro Block Chain a Pepe, debería quedar con una billetera de 51" $ saldoEnCiertoPuntoDeBlockChain 3 blockChain pepe `shouldBe` nuevaBilletera 51 pepe
    it "28. Aplicamos nuestro Block Chain a Pepe y Lucho. Pepe debería quedar con una billetera de 115 y Lucho con 0" $ (sum.(map billetera).aplicarBlockChainAConjuntoDeUsuarios blockChain) usuarios `shouldBe` 115
    it "29. Deberían ser 11 bloques de nuestra Block Chain para que Pepe supere los 10000 en su billetera" $ cuantosBloquesNecesito pepe blockChainInfinita 1 `shouldBe` 11
 -- Para resolver esto último se hace uso del concepto de evaluación diferida, la cual implica que al generar una lista infinita, se procesarán los elementos a medida que se generan.
    -- En este caso, se tomaron elementos de una blockchain infinita hasta que se dio la condición de que el saldo sea mayor a 10000.

type Bloque = [Transaccion]

bloque1 :: Bloque
bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]

bloque2 :: Bloque
bloque2 = [transaccion2,transaccion2,transaccion2,transaccion2,transaccion2]

type Usuarios = [Persona]
usuarios = [pepe,lucho]

transaccionAUsuario = map (impactarTransaccion)

aplicarBloque :: Bloque -> Persona -> Persona
aplicarBloque unBloque unaPersona= foldr ($) unaPersona (transaccionAUsuario unBloque)

usuariosConSaldoMayorANumero :: Billetera -> Bloque -> Usuarios -> Usuarios
usuariosConSaldoMayorANumero unNumero unBloque = filter ((> unNumero).billetera.aplicarBloque unBloque)

peorBloque :: BlockChain -> Persona -> Persona
peorBloque unaBlockChain unUsuario = usuarioMenosAdinerado (map (flip (aplicarBloque) unUsuario) unaBlockChain) []


elPrimeroEsMasAdinerado :: Persona -> Persona -> Bool
elPrimeroEsMasAdinerado unUsuario otroUsuario = (((<=)(billetera otroUsuario)).billetera) unUsuario

condicionMasAdinerado :: Bloque -> Usuarios -> Persona -> Bool
condicionMasAdinerado unBloque unosUsuarios unUsuario = all((elPrimeroEsMasAdinerado (aplicarBloque unBloque unUsuario)).aplicarBloque unBloque) unosUsuarios

usuarioMasAdinerado :: Usuarios -> Bloque -> Persona
usuarioMasAdinerado unosUsuarios unBloque = fromJust(find(condicionMasAdinerado unBloque unosUsuarios) unosUsuarios )


elPrimeroEsMenosAdinerado :: Persona -> Persona -> Bool
elPrimeroEsMenosAdinerado unUsuario otroUsuario = (((>=)(billetera otroUsuario)).billetera) unUsuario

condicionMenosAdinerado :: Bloque -> Usuarios -> Persona -> Bool
condicionMenosAdinerado unBloque unosUsuarios unUsuario = all((elPrimeroEsMenosAdinerado (aplicarBloque unBloque unUsuario)).aplicarBloque unBloque) unosUsuarios

usuarioMenosAdinerado :: Usuarios -> Bloque -> Persona
usuarioMenosAdinerado unosUsuarios unBloque = fromJust(find(condicionMenosAdinerado unBloque unosUsuarios) unosUsuarios )



type BlockChain = [Bloque]
blockChain :: BlockChain
blockChain = bloque2 : take 10 (repeat bloque1)

aplicarBlockChainAUsuario :: BlockChain -> Persona -> Persona
aplicarBlockChainAUsuario unaBlockChain = aplicarBloque (concat unaBlockChain)

saldoEnCiertoPuntoDeBlockChain :: Int -> BlockChain -> Persona -> Persona
saldoEnCiertoPuntoDeBlockChain unNumero unaBlockChain = aplicarBlockChainAUsuario (take unNumero unaBlockChain)

aplicarBlockChainAConjuntoDeUsuarios :: BlockChain -> Usuarios -> Usuarios
aplicarBlockChainAConjuntoDeUsuarios unaBlockChain = map (aplicarBloque (concat unaBlockChain))

generarBlockChainInfinita :: Bloque -> BlockChain
generarBlockChainInfinita unBloque = unBloque : generarBlockChainInfinita (unBloque ++ unBloque)

blockChainInfinita :: BlockChain
blockChainInfinita = generarBlockChainInfinita bloque1

cuantosBloquesNecesito unaPersona unaBlockChain unNumero | billetera unaPersona < 10000 = cuantosBloquesNecesito (saldoEnCiertoPuntoDeBlockChain unNumero unaBlockChain unaPersona) unaBlockChain (unNumero + 1)
                                                         | otherwise = unNumero


--listopio = [12,1,7,7,5,1,9,2,12,3,4,5,5]
--esMenor [] = []
--esMenor (cabeza:cola) = find (all ((<=) cabeza)) [(cabeza:cola)] : esMenor cola
