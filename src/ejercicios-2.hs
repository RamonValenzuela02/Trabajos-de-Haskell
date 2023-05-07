--ejercicio 1
siguiente:: Num a=>a -> a
siguiente num= num +1

--ejercicio 2
mitad:: Float-> Float
mitad num= num / 2

--ejercicio 3
inversa::Float-> Float
inversa num= 1 / num

--ejercicio 4
triple:: Num a=>a -> a
triple =(*3)

--ejercicio 5
esNumeroPositivo::(Ord a,Num a)=>a ->Bool
esNumeroPositivo num= num>=0 

--ejercicio 6
--no se como hacer composicion en esta funcion
--esMultiplodeDos:: (Ord a,Num a)=>a-> a -> a -> Bool
--esMultiplodeDos = 


--ejercicio 7
esBisiesto:: Int -> Bool
esBisiesto anio =  (esdivisiblepor 400 anio  || esdivisiblepor 4 anio )&&(not.esdivisiblepor 100 $ anio)

esdivisiblepor:: Int -> Int -> Bool
esdivisiblepor divisor anio= mod anio divisor==0

--ejercicio 8
inversaRaizCuadrada:: Float-> Float
inversaRaizCuadrada = inversa.sqrt 

--ejercicio 9
incrementMCuadradoN:: Num a=>a ->a ->a
incrementMCuadradoN m  = (+m).(^2)

--ejercicio 10
esResultadoPar:: Integral a=> a -> a -> Bool
esResultadoPar n m= even.(^m) $ n

--ejercicio 11

