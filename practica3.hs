module Practica3 where

-- 1 Introducción ---------------------------------------------------------------------------------

type ID = String

data LNLambda
  = Bvar Int
  | Fvar ID
  | AbsL LNLambda
  | AppL LNLambda LNLambda
  deriving (Eq)

-- Instancia la clase Show para el tipo de dato LNLambda.
instance Show LNLambda where
  show (Bvar n) = show n
  show (Fvar x) = show x
  show (AbsL e) = "(\\." ++ show e ++ ")"
  show (AppL e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

-- 2 Manipulación de Abstracciones --------------------------------------------------------------
type SubstL = (Int, ID)

-- Define la función varOpen e s que devuelve una copia de e en la que todas las variables ligadas que se refieren a la abstracción más externa son remplazadas por la variable indicada en s.
varOpen :: LNLambda -> SubstL -> LNLambda
varOpen (Bvar n) (i, x) = if n == i then Fvar x else Bvar n
varOpen (Fvar y) _ = Fvar y
varOpen (AbsL e) (i, x) = AbsL (varOpen e (i + 1, x))
varOpen (AppL e1 e2) s = AppL (varOpen e1 s) (varOpen e2 s)

-- Define la función varClose e s que devuelve una copia de e en la que todas las apariciones de la variable indicada por s han sido reemplazadas por una variable ligada.
varClose :: LNLambda -> SubstL -> LNLambda
varClose (Bvar n) _ = Bvar n
varClose (Fvar y) (i, x) = if y == x then Bvar i else Fvar y
varClose (AbsL e) (i, x) = AbsL (varClose e (i + 1, x))
varClose (AppL e1 e2) s = AppL (varClose e1 s) (varClose e2 s)

-- 3 Normalización -------------------------------------------------------------------------------
