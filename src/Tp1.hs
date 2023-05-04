--ejercicio de TP

data Sustancia=
     Elemento{
   tipo::String,
   nombre::String,
   simbolo::String,
   numeroAt::Int,
   grupo::String
}
    |Compuesto{
   componentes::[(Sustancia,Int)],
   formula::String,
   nombre::String,
   grupo::String
}deriving (Show)

hidrogeno::Sustancia
hidrogeno=Elemento "Sencillo" "hidrogeno" "H" 1 "no metal"

oxigeno::Sustancia
oxigeno=Elemento "Sencillo" "oxigeno" "O" 8 "no metal"

agua::Sustancia
agua=Compuesto [(oxigeno,1), (hidrogeno,2)] "H2O" "agua" "no metal"

--2
conduceBien::Sustancia -> Bool
conduceBien (Elemento _ _ _ _ grupo)
   |grupo=="no metal"= True
   |otherwise=False
conduceBien(Compuesto _ _ _ grupo)
  |grupo=="no metal"=True
  |otherwise= False

--3

vocales=['a','e','i','o','u']

agregoUro::Sustancia -> String
agregoUro (Elemento _ nombre _ _ _ )= terminaEnConsonante nombre
agregoUro(Compuesto _ _ nombre _)=terminaEnConsonante nombre

terminaEnConsonante::String -> String
terminaEnConsonante nombre
    |filter (==last nombre)vocales==[] =nombre ++"uro"
    |otherwise= terminaEnConsonante (take (length nombre -1)nombre)


--4

cloro::Sustancia
cloro= Elemento "Sencillo" "cloro" "Cl" 17 "no metal"

sodio::Sustancia
sodio=Elemento "Sencillo" "sodio" "Na" 11 "no metal"


unionNombres::Sustancia -> Sustancia -> String
unionNombres sustanciaUno (Elemento _ nombreDos _ _ _)=agregoUro sustanciaUno ++ " de " ++ nombreDos
unionNombres sustanciaUno (Compuesto _ _ nombreDos _)=agregoUro sustanciaUno ++ " de " ++ nombreDos 


--5
mezclar:: (Sustancia,Int) -> (Sustancia,Int) -> Sustancia 
mezclar (sustanciaUno,cantMolesUno)(sustanciaDos,cantMolesDos)= (Compuesto [(sustanciaUno,cantMolesUno),(sustanciaDos,cantMolesDos)] ((calculoFormula2 sustanciaUno cantMolesUno)++(calculoFormula2 sustanciaDos cantMolesDos)) (unionNombres sustanciaUno sustanciaDos) "no metal") 


calculoFormula2:: Sustancia -> Int -> String
calculoFormula2 (Elemento _ _ simbolo _ _) moles
    |moles==1 =simbolo
    |moles/=1 =simbolo ++ show(moles)
calculoFormula2 (Compuesto _ simbolo _ _) moles 
    |moles==1 =simbolo
    |moles/=1 =simbolo ++ show(moles)