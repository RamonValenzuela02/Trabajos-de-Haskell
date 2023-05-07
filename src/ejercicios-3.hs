
import Data.List

--ejercicio 1
data Numeros=Numero{
    num1::Int,
    num2::Int,
    num3::Int
}

ejemplo:: Numeros
ejemplo =Numero 1 2 3

fst3::Numeros -> Int 
fst3 = num1

snd3::Numeros -> Int
snd3=num2

trd3::Numeros -> Int 
trd3 = num3

---------------
--ejercicio 2
aplicar::Ord a=>a ->(a->b)->(a->c)->(b ,c)
aplicar parametro funcion1 funcion2 = (funcion1 parametro, funcion2 parametro )

---------------
--ejercicio 3
--no se si esta bien o es muy algotmico

cuentaBizarra::(Ord a, Num a)=>(a,a)-> Num a=>a
cuentaBizarra tupla 
    |fst tupla > snd tupla     = fst tupla + snd tupla 
    |snd tupla-fst tupla >=10  = snd tupla -fst tupla 
    |otherwise                 = fst tupla * snd tupla 




---------------
--ejercicio 4
--falta la parte d que no la entendi muy bien 
esNotaBochazo::(Num a,Ord a)=>[a] -> Bool
esNotaBochazo = null.filter (>=6)

aprobo::(Num a,Ord a)=>[a] -> Bool
aprobo = not.esNotaBochazo 

promociono::(Num a,Ord a)=>[a] -> Bool
promociono lista = (sum lista >=15)&&(null.filter(<8) $ lista)



--ejercicio 5
--no entiendo el b y c a que se refiere con consulta 
notasFinales::(Num a, Ord a)=>[[a]] -> [a]
notasFinales = (drop 2).sort.concat



--ejercicio 6

