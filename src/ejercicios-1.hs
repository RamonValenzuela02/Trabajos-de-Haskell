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




