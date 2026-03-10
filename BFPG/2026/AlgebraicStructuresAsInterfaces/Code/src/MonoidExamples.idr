module MonoidExamples
import SemiGroup
import Monoid

%hide Prelude.Monoid
%default total

-- ε · True
-- Idris give us the mechanism to specify which implementation 
-- to use at the call site.
ex1 : Bool
ex1 = · @{sgAnd} (ε @{monoidBand}) True

ex1_prf : MonoidExamples.ex1 = True
ex1_prf = axiomLeftId @{monoidBand} True
 
ex2 : Bool
ex2 = · @{sgOr} False (ε @{monoidBor})

ex2_prf : MonoidExamples.ex2 = False
ex2_prf = axiomRightId @{monoidBor} False

-- ε · 2 where · = + and ε = 0
ex3 : Nat
ex3 = · @{sgAdd} (ε @{monoidAdd}) 2

ex3_prf : MonoidExamples.ex3 = 2
ex3_prf = axiomLeftId @{monoidAdd} 2

ex4 : Nat
ex4 = · @{sgMult} 2 (ε @{monoidMult})

ex4_prf : MonoidExamples.ex4 = 2
ex4_prf = axiomRightId @{monoidMult} 2



