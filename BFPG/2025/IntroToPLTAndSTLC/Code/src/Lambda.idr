module Lambda

V : Type
V = String

data Λ = Var V 
       | App Λ Λ
       | Abs V Λ

id : Λ
id = Abs "x" (Var "x")

const : Λ
const = Abs "a" (Abs "b" (Var "a"))
