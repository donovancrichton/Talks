module ForeverExample
import Data.Vect

f : Nat -> Nat
f x = f x

v : Vect k a -> Vect (f k) a
v xs = xs
