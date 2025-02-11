module Nat where

data Nat = Z | S Nat

zero :: Nat
zero = Z

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S two
