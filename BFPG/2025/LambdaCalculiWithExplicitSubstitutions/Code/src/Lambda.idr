module Lambda

-- Our 'set' of variables.
V : Type
V = String

-- Our Lambda (Λ) 
-- Calculus Syntax.
data Λ = Var V 
       | App Λ Λ
       | Abs V Λ

