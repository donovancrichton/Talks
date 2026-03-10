module SemiGroupExamples
import SemiGroup

%default total

-- Examples with Boolean operations.
ex1 : Bool
ex1 = · @{sgAnd} True False

ex1_prf : SemiGroupExamples.ex1 = False
ex1_prf = Refl

ex2 : Bool
ex2 = · @{sgOr} True False

ex2_prf : SemiGroupExamples.ex2 = True
ex2_prf = Refl

-- Examples with Natural Number operations.
ex3 : Nat
ex3 = · @{sgAdd} 1 2

ex3_prf : SemiGroupExamples.ex3 = 3
ex3_prf = Refl

ex4 : Nat
ex4 = · @{sgMult} 1 2

ex4_prf : SemiGroupExamples.ex4 = 2
ex4_prf = Refl
