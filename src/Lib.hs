import Data.Bifoldable (biList)

doble :: Int -> Int 
doble hola = 2 *  hola 

espar:: Int -> Bool
espar num= mod num 2 == 0

eslongpar:: String -> Bool
eslongpar palabra = espar(length palabra)


suma ::Int -> Int -> Int 
suma num1 num2= num1 + num2 
--ejercicios de la guia

--ejercicio 1
esMultiplodeTres:: Int -> Bool
esMultiplodeTres num = mod num 3== 0  

--ejercicio 2
esMultiplode:: Int -> Int -> Bool
esMultiplode dividendo divisor = mod dividendo divisor==0

--ejercicio 3
cubo:: Num a=>a -> a
cubo num=num *num*num

--ejercicio 4
area:: Num a=>a ->a ->a
area base altura = altura * base

--ejercicio 5
esBisiesto:: Int->Bool
esBisiesto anio=(mod anio 400==0 || mod anio 4==0)&&(mod anio 100 /=0)

--ejercicio 6
celsiusToFahr:: Float->Float
celsiusToFahr celcius = (celcius * 1.8) +32

--ejercicio 7
fahrToCelsius:: Float -> Float
fahrToCelsius fahr=  (fahr -32)* (5/9)

--ejercicio 8
haceFrio::  Float-> Bool
haceFrio fahr= fahr < 8.0

--ejercicio 9
mcm_dos:: Int-> Int -> Int
mcm_dos dividendo divisor= div(divisor * dividendo) (gcd dividendo divisor)

--ejercicio 10
max3:: Ord a=>a->a->a->a
max3 num1 num2 num3= max num1 (max num2 num3)
min3:: Ord a=>a->a->a->a
min3 num1 num2 num3= min num1 (min num2 num3)


dispersion:: (Ord a,Num a)=>a->a->a->a
dispersion num1 num2 num3= max3 num1 num2 num3 - min3 num1 num2 num3 
 
dispersion2:: Int->Bool
dispersion2 num = num>=30
--ejercicio 11


--ejercicio 12



--viendo los ejercicios de la clase (clase 4)
--listas

notasRamon:: [Int]
notasRamon = [9, 5, 7]

esNotamayor:: Int ->Bool
esNotamayor nota= nota>=6

notasAprobadas:: [Int]->[Int]
notasAprobadas lista= filter esNotamayor lista 



esLongPar:: String -> Bool
esLongPar palabra= even (length palabra)

palabraPar:: [String]->[String]
palabraPar lista = filter esLongPar lista 


longPalabra:: [String]-> [Int]
longPalabra lista = map length lista


funcionClase::[Int]->[Int]
funcionClase lista=(filter (100>)) (map((+40).(*8)) lista) 

funcionClase2::[Int]->[Int]
funcionClase2 lista= (filter (100>) .map((+40).(*8))) lista

funcionClase3::[Int]->[Int]
funcionClase3 = filter (100>) .map((+40).(*8)) 

funcionLoca:: (b->Bool)->(a->b)->([a]->[b])
funcionLoca f g =filter f . map g


funcion::Bool->String
funcion True = "hola como estas"
funcion False = "no puedo responder "


--ejercicios de composicion y aplicacion parcial
--guia 2

--ejercicio 1
siguiente::Int-> Int
siguiente = (+1)

--ejercicio 2
mitadNum:: Fractional a=>a->a 
mitadNum = (/2)

mitadInt:: Integral a=>a->a
mitadInt num = div num 2

--ejercicio 3


--ejercicio 4
triple:: Num a=>a->a
triple num= num*num*num

--ejercicio 5



--provando las datas

data Estudiante = UnEstudiante{
    nombre2:: String,
    nota :: Int,
    legajo:: String
}


juanita :: Estudiante
juanita =UnEstudiante "Juana" 8 "123456-7"

hilario :: Estudiante
hilario = UnEstudiante "hilario" 6 "345545-5"

aprobo2:: Estudiante ->Bool
aprobo2 estudiante = nota estudiante>=6 






--usando el Pattern maching en datas
--de esta manera sacamos mas de un valor de una data
legNomConcat:: Estudiante ->String
legNomConcat (UnEstudiante nombre _ legajo )= nombre ++ " "++ legajo  


notaMayor1ra:: Estudiante -> Estudiante -> Bool
notaMayor1ra (UnEstudiante _ nota1 _) (UnEstudiante _ nota2 _)= nota1>nota2


--ejercicios del video

retorMasGran:: String -> String -> String
retorMasGran string1 string2
    |length string1 < length string2 = string2
    |otherwise= string2 


calculoAbsolut:: (Num a, Ord a)=>a->a
calculoAbsolut num
   |num >=0 = num
   |otherwise =num * (-1)


calculoMayor:: (Num a, Ord a)=>a->a->a
calculoMayor num1 num2
    |calculoAbsolut num1 < calculoAbsolut num2 = num2
    |otherwise = num1




--1) 
{-
data Componente = Componente {
    sustancia     :: Sustancia,
    cantMoleculas :: Int
} deriving (Show)
-}

data Sustancia 
    = Elemento {
        tipo      :: String,
        nombre    :: String,
        simbolo   :: String,
        numAtomic :: Int,
        grupo     :: String
    }  
    | Compuesto{
        componentes :: [(Sustancia , Int)],
        nombre      :: String,
        formula     :: String,
        grupo       :: String
    } deriving (Show)

vocales :: [Char]
vocales = ['a', 'e', 'i', 'o', 'u']

hidrogeno :: Sustancia
hidrogeno =  Elemento {
    tipo         = "Sencillo",
    nombre       = "Hidrogeno",
    simbolo      = "H",
    numAtomic    = 1,
    grupo        = "No metal"
}

oxigeno :: Sustancia
oxigeno =  Elemento {
    tipo         = "Sencillo",
    nombre       = "Oxigeno",
    simbolo      = "O",
    numAtomic    = 8,
    grupo        = "No metal"
}

agua :: Sustancia
agua = Compuesto {
    componentes = [(oxigeno,1), (hidrogeno,2)],
    nombre = "Agua",
    formula = "H2O",
    grupo = "No metal"
}



cloro :: Sustancia
cloro =  Elemento {
    tipo         = "Sencillo",
    nombre       = "Cloro",
    simbolo      = "Cl",
    numAtomic    = 17,
    grupo        = "No metal"
}

sodio :: Sustancia
sodio =  Elemento {
    tipo         = "Sencillo",
    nombre       = "Sodio",
    simbolo      = "Na",
    numAtomic    = 11,
    grupo        = "No metal"
}

cloruroSodio :: Sustancia
cloruroSodio = Compuesto {
    componentes = [( cloro,1), (sodio,1)],
    nombre = "cloruro de sodio",
    formula = "NaCl",
    grupo = "No metal"
}

--2)

conduccion :: Sustancia -> Bool
conduccion (Elemento _ _ _ _ grupo) 
    |grupo     == "No metal" = False
    |otherwise = True
conduccion (Compuesto _ _ _ grupo)
    |grupo     == "No metal" = False
    |otherwise = True

--3)

agregoUro :: Sustancia -> String
agregoUro (Elemento _ nombre _ _ _) =  filtroHastaUltimaConsonante nombre   
agregoUro (Compuesto _ nombre _ _) =  filtroHastaUltimaConsonante nombre

filtroHastaUltimaConsonante :: String -> String
filtroHastaUltimaConsonante palabra
    |revisoUltimaConsonante palabra == True = palabra ++ "uro"
    |otherwise = filtroHastaUltimaConsonante (take ((length palabra)-1) palabra)

revisoUltimaConsonante :: String -> Bool
revisoUltimaConsonante palabra = esConsonante (last palabra)

esConsonante :: Char -> Bool
esConsonante ultimaLetra =(filter (==ultimaLetra) vocales == [] )


--4)

combinar :: Sustancia -> Sustancia -> String
combinar sustanciaUno (Elemento _ nombreDos _ _ _) = agregoUro sustanciaUno ++ " de " ++ nombreDos
combinar sustanciaUno (Compuesto _ nombreDos _ _ ) = agregoUro sustanciaUno ++ " de " ++ nombreDos


--5)

mezclar :: (Sustancia , Int) -> (Sustancia , Int) -> Sustancia
mezclar (sustanciaUno, cantMoleculasUno) (sustanciaDos, cantMoleculasDos)
    = (Compuesto [(sustanciaUno,cantMoleculasUno),(sustanciaDos,cantMoleculasDos)] (combinar sustanciaUno sustanciaDos) ((obtengoFormula sustanciaUno cantMoleculasUno) ++ (obtengoFormula sustanciaDos cantMoleculasDos)) "No metal")


obtengoFormula :: Sustancia -> Int -> String
obtengoFormula (Elemento _ _ formula _ _) moleculas  
    |moleculas == 1 = formula 
    |moleculas /= 1 = formula ++ (show moleculas)
obtengoFormula (Compuesto _ _ formula _)  moleculas 
    |moleculas == 1 = formula 
    |moleculas /= 1 = formula ++ (show moleculas)

pruebaAgua :: Sustancia
pruebaAgua = Compuesto {
    componentes = [(hidrogeno,2),(oxigeno,1)],
    nombre      = "agua",
    formula     = "H20",
    grupo       = "No metal"
}

pruebaCloruroSodio :: Sustancia
pruebaCloruroSodio = Compuesto {
    componentes = [(cloro,1),(sodio,1)],
    nombre      = "Cloruro de sodio",
    formula     = "NaCl",
    grupo       = "No metal"
}
{-

--6

formulaSeis :: Sustancia -> String
formulaSeis (Elemento _ _ simbolo _ _) = simbolo

formulaSeis (Compuesto [componentes] _ _ _) = leoHastaUltimoComponente [componentes]

leoHastaUltimoComponente :: (Sustancia , Int) -> String
leoHastaUltimoComponente [componentes]
    |snd(last [componentes]) == 1 = (obtengoFormula (fst(last [componentes]))) ++ leoHastaUltimoComponente (take ((length [componentes])-1) [componentes])
    |snd(last [componentes]) /= 1 = (obtengoFormula (fst(last [componentes]))) ++ (show (snd(last [componentes]))) ++ leoHastaUltimoComponente (take ((length [componentes])-1) [componentes])

--- falta arreglar la ultima funcion, la idea es que imprima la formula del ultimo componente junto a la cantidad de moleculas y que avance al semiultimo y repita....
--- en este formato cada indice del vector componentes posee una sustancia y la cantidad de moleculas
--- Al hacer snd(last[componentes]) la idea seria agarrar el segundo valor, cantidad de moleculas, y compararlo, para saber si es o no mayor a uno.
---
-}