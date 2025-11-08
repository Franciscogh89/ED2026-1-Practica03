module LabDis where

-- Sintaxis de la lógica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

-- Para que se vea bien en la terminal
instance Show Prop where 
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Algunas variables para no estar escribiendo mucho en la terminal
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Tipo para representar una asignación de valores de verdad (modelo)
type Modelo = [String]

-- ============================================================
-- FUNCIONES AUXILIARES
-- ============================================================

-- Eliminar duplicados de una lista
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Obtener todas las variables de una fórmula
variables :: Prop -> [String]
variables (Var x) = [x]
variables (Cons _) = []
variables (Not f) = variables f
variables (And f1 f2) = variables f1 ++ variables f2
variables (Or f1 f2) = variables f1 ++ variables f2
variables (Impl f1 f2) = variables f1 ++ variables f2
variables (Syss f1 f2) = variables f1 ++ variables f2

-- Generar todos los posibles modelos (subconjuntos de variables)
todosLosModelos :: [String] -> [Modelo]
todosLosModelos [] = [[]]
todosLosModelos (v:vs) = [modelo | modelo <- todosLosModelos vs] ++ 
                         [v:modelo | modelo <- todosLosModelos vs]

-- Evaluar una fórmula dado un modelo
evaluar :: Prop -> Modelo -> Bool
evaluar (Var x) modelo = x `elem` modelo
evaluar (Cons b) _ = b
evaluar (Not f) modelo = not (evaluar f modelo)
evaluar (And f1 f2) modelo = evaluar f1 modelo && evaluar f2 modelo
evaluar (Or f1 f2) modelo = evaluar f1 modelo || evaluar f2 modelo
evaluar (Impl f1 f2) modelo = not (evaluar f1 modelo) || evaluar f2 modelo
evaluar (Syss f1 f2) modelo = evaluar f1 modelo == evaluar f2 modelo

-- ============================================================
-- FUNCIONES PRINCIPALES
-- ============================================================

-- Obtener todos los modelos que satisfacen una fórmula
modelos :: Prop -> [Modelo]
modelos formula = filter (evaluar formula) (todosLosModelos vars)
  where vars = nub (variables formula)

-- Verificar si dos fórmulas son equivalentes
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f1 f2 = modelosOrdenados f1 == modelosOrdenados f2
  where
    -- Obtener todas las variables de ambas fórmulas
    todasVars = nub (variables f1 ++ variables f2)
    -- Generar todos los posibles modelos
    todosModelos = todosLosModelos todasVars
    -- Filtrar modelos que satisfacen cada fórmula
    modelosOrdenados f = filter (evaluar f) todosModelos

