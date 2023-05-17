import Data.List 
import GHC.Arr (listArray)



--ejercicios de la guia 4 de paradigma funcional 

--ejercicio 1
--el ejercicio 1 es basicamente la funcion summ


--ejercicio 2
--averigua la division entre enteros y flotantes 

--a
--promedioFrecuenciaCardiaca::[Int]->Float   
--promedioFrecuenciaCardiaca lista = (/ length lista).sum $ lista



frecuenciaCardiacaMinuto:: Int -> [Int] ->Int
frecuenciaCardiacaMinuto tiempo listaFrecCard = listaFrecCard !! calculoUbi tiempo 

calculoUbi:: Int -> Int 
calculoUbi tiempo= (div tiempo 10)


frecuenciasHastaMomento:: Int -> [Int] -> [Int]
frecuenciasHastaMomento tiempo = take ((calculoUbi tiempo)+1)




------------
--ejercicio 3
esCapicua::[String]->Bool
esCapicua listaCapicua = concat listaCapicua == reverse (concat listaCapicua) 


--ejercicio 4
data Tipo = HorarioReducido | HorarioNormal deriving (Show, Eq)

data Llamada= Llamada{
    tipo::Tipo,
    vectorLlamadas::[Int]
}
ejemplo::Llamada
ejemplo = Llamada HorarioReducido [1,1,1]

ejemplo2::Llamada
ejemplo2 = Llamada HorarioNormal [2,2,2,2]

ejemplo3 = [ejemplo,ejemplo2]

cuandoHabloMasMinutos:: [Llamada] ->String 
cuandoHabloMasMinutos llamadasRecib
        |(sum.sacoVector.head $ llamadasRecib) > (sum.sacoVector.last $ llamadasRecib) = show(tipo (head llamadasRecib)) 
        |otherwise = show(tipo (last llamadasRecib))

sacoVector:: Llamada -> [Int]
sacoVector = vectorLlamadas 

--parte b
cuandoHizoMasLlamadas::[Llamada] -> String 
cuandoHizoMasLlamadas llamadasRecib
        |(length.sacoVector.head $ llamadasRecib) > (length.sacoVector.last $ llamadasRecib) = show(tipo (head llamadasRecib)) 
        |otherwise = show(tipo (last llamadasRecib))




------------
--PARTE ORDEN SUPERIOR

--ejercicio 1
existsAny:: (a->Bool) -> [a] ->Bool
existsAny  = any  

--ejercicio 2 (no se como declararla
{- 
mejor:: Ord a=> (a->b) -> (a->c) -> a -> 
mejor funcion1 funcion2 valor 
        |funcion1 valor >= funcion2 valor = funcion1 valor 
        |otherwise = funcion2 valor   

-}

---ejercicio 3 
aplicarPar:: (Num a,Ord a)=>(a->c)->(a,a)->(c,c)
aplicarPar funcion (x,y)= (funcion x,funcion y)



---------------------------
--ORDEN SUPERIOR ++ LISTAS

--ejercicio 1
esMultiploDeAlguno:: Int -> [Int] ->Bool
esMultiploDeAlguno num lista =any (multiplo num) lista 

multiplo::Int -> Int -> Bool
multiplo num numLista = mod numLista num ==0 


--ejercicio 2
promedios:: [[Int]]->[Int]
promedios []=[]
promedios (x:xs)= calculoPromedio x ++ promedios xs

calculoPromedio::[Int]->[Int]
calculoPromedio lista = [div (sum lista) (length lista)]


--ejercicio 3
promediosSinAplazos:: [[Int]] -> [Int]
promediosSinAplazos lista= filter (>4) (promedios lista) 

--opcion con map
promediosSinAplazosAlternativa:: [[Int]] -> [Int]
promediosSinAplazosAlternativa lista= filter (>4) (calculoPromConMap lista) 

calculoPromConMap::[[Int]]->[Int]
calculoPromConMap lista= map hagoCalculo lista 

hagoCalculo::[Int] -> Int
hagoCalculo lista = div (sum lista) (length lista)

--ejercicio 4
mejoresNotas::[[Int]] -> [Int]
mejoresNotas []=[]
mejoresNotas (x:xs)= [last.sort $ x] ++ mejoresNotas xs

mejoresNotasAlternativa::[[Int]] -> [Int]
mejoresNotasAlternativa []=[]
mejoresNotasAlternativa (x:xs)= [maximum x] ++ mejoresNotas xs


--ejercicio 5
aprobo:: [Int] -> Bool
aprobo lista = div (sum lista) (length lista) >=6
--no se como usar la funcion minimum aca 

--ejercicio 6
aprobaron::[[Int]] -> [[Int]]
aprobaron lista = filter aprobo lista 

--ejercicio 7

divisores:: Int -> [Int]
divisores num =filter (esDivisible num) [1..num]

esDivisible:: Int -> Int -> Bool
esDivisible valor divisor = mod valor divisor ==0

--ejercicio 8
exists:: (a->Bool) -> [a] ->Bool
exists  = any


--ejercicio 9 (no entiendo a q se refiere con "algun algo")


--ejercicio 10 
aplicarFunciones::[a->b] -> a -> [b]
aplicarFunciones [] _=[]
aplicarFunciones (funcionDeLaHead:funcionDeLaTail) num = [funcionDeLaHead num] ++  (aplicarFunciones funcionDeLaTail num)

--ejercicio 11
sumaF::(Num a)=>(Num b)=>[a->b] -> a -> b
sumaF [] _=0
sumaF (funcionDeLaHead:funcionDeLaTail) num=  (funcionDeLaHead num) + (sumaF funcionDeLaTail num)


--no se por que no  me compila 
--otra opcion 

--ejercicio 12
subirHabilidad::Int -> [Int] -> [Int]
subirHabilidad _ [] =[]
subirHabilidad num (x:xs)= [(hagoSuma x num)] ++ (subirHabilidad num xs)

hagoSuma:: Int -> Int ->Int
hagoSuma valorJugador cantidadSuma
        |valorJugador + cantidadSuma>12 = 12
        |otherwise = valorJugador + cantidadSuma

--ejercicio 13
flimitada:: (Num a,Ord a) => (Num b,Ord b) => (a->b) -> a ->b
flimitada funcion num 
        |(funcion num) < 0  = 0
        |(funcion num) > 12 =12
        |otherwise = funcion num


--ejercicio 14
--aca hay q probar la funcion takeWhile
--te hace una lista nueva con los valores que cumplieron hasta que llego un caso q no cumplio (arranca de la ubicacion cero)


--ejercicio 15
primerosPares:: [Int] -> [Int]
primerosPares = takeWhile even 


primerosDivisores::Int -> [Int] -> [Int]
primerosDivisores num lista = takeWhile (esDivisor num)$lista

esDivisor::Int -> Int -> Bool
esDivisor dividendo divisor = mod dividendo divisor ==0


primerosNoDivisores::Int -> [Int] -> [Int]
primerosNoDivisores num lista = takeWhile (noEsDivisor num)$lista

noEsDivisor::Int -> Int -> Bool
noEsDivisor dividendo divisor = mod dividendo divisor /=0

--ejercicio 16 (no lo entendi )


--ejercicio 17

crecimientoAnual:: Int -> Int
crecimientoAnual edad = calculoCuantoCrece edad 

calculoCuantoCrece::Int -> Int
calculoCuantoCrece edad 
        |edad<=10 = 24 - (edad * 2)
        |(edad>10 && edad<=15) = 4
        |(edad>15 && edad<=17) = 2
        |(edad>17 && edad<=19) = 1
        |otherwise             = 0

crecimientoEntreEdades:: Int -> Int -> Int
crecimientoEntreEdades edadMenor edadMayor = calculoCrecimiento [edadMenor..edadMayor-1]

calculoCrecimiento:: [Int]->Int 
calculoCrecimiento []=0
calculoCrecimiento (x:xs)= calculoCuantoCrece x + calculoCrecimiento xs

crecimientoEntreEdadesDos:: Int -> Int -> Int
crecimientoEntreEdadesDos edadMenor edadMayor = sum (map (calculoCuantoCrece) [edadMenor..edadMayor-1])


alturasEnUnAnio:: Int -> [Int] -> [Int]
alturasEnUnAnio edad listaAlturas = map (+calculoCuantoCrece edad) listaAlturas

alturaEnEdades::Int -> Int -> [Int] -> [Int]
alturaEnEdades _ _ []=[]
alturaEnEdades altura edad (x:xs) = [altura + sum(map (calculoCuantoCrece) [edad..x-1])] ++ alturaEnEdades altura edad xs 
--algo raro esta pasando en esta funcion 


--ejercicio 18
rachasLluvia:: [Int] -> [[Int]]
rachasLluvia []=[]
rachasLluvia listaLluvias = [calculoSeguidillaRain listaLluvias] ++ rachasLluvia((sacoSeguidilla listaLluvias))

sacoSeguidilla:: [Int] -> [Int]
sacoSeguidilla  lista 
         | head lista==0 =dropWhile (==0) lista
         |otherwise = dropWhile (/=0) lista


calculoSeguidillaRain:: [Int]-> [Int]
calculoSeguidillaRain = takeWhile (/=0)

lluviasEnero :: [Int]
lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

--ejercicio 19

--ejercicio 20

--ejercicio 21 