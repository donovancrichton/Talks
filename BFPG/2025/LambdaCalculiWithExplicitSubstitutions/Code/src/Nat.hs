module Nat where

data Nat = Z | S Nat

three :: Nat
three = S (S (S Z))
