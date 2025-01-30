module Forall
import Data.Nat
-- forall x where x is a Natural Number, 
-- x is greater than 0.
f : (x : Nat) -> 0 `LTE` x
f Z = LTEZero
f (S k) = lteSuccRight (f k)
