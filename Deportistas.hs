module Deportistas where

---------------------------------------------
-- 1) Modelar las bebidas energéticas, de forma que se puedan agrupar fácilmente muchas de ellas, como por ejemplo en una lista.

data BebidaEnergetica = Powerade | Gatorade  | Agua {conGas::Bool} | Speed deriving Show

---------------------------------------------
-- 2) Modelar el comportamiento que permita saber cuánta energía te da una bebida.

energiaBebida Powerade = 100
energiaBebida Gatorade = 130
energiaBebida (Agua False) = 50
energiaBebida _ = 3


{-
De los futbolistas, conocemos el nombre, su energía, el botín que usan, la cantidad de partidos que jugaron hasta el momento y todos los equipos en los que jugaron durante su carrera. 
De los botines de un jugador sabemos la cantidad de goles que se hicieron con ellos, las asistencias realizadas y la precisión que el jugador posee cuando los usa.
De los basquetbolistas conocemos su nombre, su cantidad de energía, su equipo actual, su nivel de puntería, y si es un jugador de selección.
Por último, de los rugbiers conocemos su nombre, la cantidad de equipos en que jugaron, el peso, la cantidad de tackles que realizaron y la agresividad del deportista.-}

data Deportista = Futbolista { 
        nombre :: String,
        energia :: Int,
        botin :: Botin,
        cantPartidos :: Int,
        equipos :: [String] } | 
    Basquetbolista {
        nombre :: String,
        energia :: Int,
        equipo :: String,
        punteria :: Int,
        esDeSeleccion :: Bool } | 
    Rugbier {
        nombre :: String,
        cantEquipos :: Int,
        peso :: Int,
        cantTackles :: Int,
        agresividad :: Int } deriving Show

data Botin = Botin {
    cantGoles::Int, 
    cantAsistencias::Int,
    precision::Int
    } deriving Show

messeh = Futbolista { 
        nombre = "Lío",
        energia = 200,
        botin = Botin {cantGoles = 100, cantAsistencias = 300, precision = 1000},
        cantPartidos = 600,
        equipos = ["Newells","Barza"] } 
mano = Basquetbolista {
        nombre = "Manu",
        energia = 200,
        equipo = "Spurs",
        punteria = 9,
        esDeSeleccion = True } 
marto = Rugbier {
        nombre = "Landajo",
        cantEquipos = 3,
        peso = 82,
        cantTackles = 13,
        agresividad = 5 }

beber bebida deportista = deportista {energia = energia deportista + energiaBebida bebida}

---------------------------------------------
-- 3) Que los deportistas puedan cambiar de equipo, teniendo en cuenta la estructura de cada deportista.
    
cambiarEquipo equipo (Futbolista nombre energia botin cantPartidos equipos) = 
    Futbolista nombre energia botin cantPartidos (equipos ++ [equipo])
cambiarEquipo equipo (Basquetbolista nombre energia equipoActual punteria esDeSeleccion) = (Basquetbolista nombre energia equipo punteria esDeSeleccion)
cambiarEquipo equipo rugbier@(Rugbier _ _ _ _ _) = rugbier {cantEquipos = cantEquipos rugbier + 1}
        
---------------------------------------------
-- 4) Modelar a los deportistas de forma que podamos ordenarlos y compararlos en base a su potencial. 
 
instance Eq Deportista where
    deportista == deportista2 = potencial deportista == potencial deportista2

instance Ord Deportista where
    deportista <= deportista2 = potencial deportista <= potencial deportista2


{- El potencial de un futbolista está dado por la siguiente cuenta:
 potencialDelBotin / (cantidadDePartidos + energia)
 Consideramos el potencial de un botín como los goles más las asistencias logradas con él, multiplicado por la precisión que el futbolista posee con dicho botín. -}

 {-
 El potencial de un basquetbolista se calcula como:
(punteria + energia) / cantidad de letras del equipo actual
Si el basquetbolista es de selección, se eleva por 2 el potencial total.
-}

{-
En el caso de un rugbier se calcula así: 
(agresividad * tackles) / cantidadDeEquipos
-}

potencial (Futbolista _ energia botin cantPartidos _ ) = div (potencialBotin botin) (cantPartidos + energia)
potencial (Basquetbolista _ energia equipo punteria esDeSeleccion) = potenciaBaseBasquetbolista punteria energia equipo ^ plusDeSeleccion esDeSeleccion
potencial (Rugbier _ cantEquipos _ cantTackles agresividad) = div (agresividad * cantTackles) cantEquipos 
    
potenciaBaseBasquetbolista punteria energia equipo = div (punteria + energia) (length equipo)

potencialBotin (Botin cantGoles cantAsistencias precision) = (cantGoles + cantAsistencias) * precision

plusDeSeleccion True = 2
plusDeSeleccion False = 1

    
    
    