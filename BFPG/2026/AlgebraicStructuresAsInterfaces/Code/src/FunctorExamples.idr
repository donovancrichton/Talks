module FunctorExamples
import Functor

%default total

ex1 : List Nat
ex1 = Functor.map id [1, 2, 3]

ex1_prf : FunctorExamples.ex1 = [1, 2, 3]
ex1_prf = axiomId [1, 2, 3]

ex2 : List Nat
ex2 = Functor.map ((+1) . (minus 1)) [1, 2, 3]

ex2_prf : FunctorExamples.ex2 = (Functor.map (+1) . Functor.map (minus 1)) [1, 2, 3]
ex2_prf = axiomCompose [1, 2, 3] (+1) (minus 1)

