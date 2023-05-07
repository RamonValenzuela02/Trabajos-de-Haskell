
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


