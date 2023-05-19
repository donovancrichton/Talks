module BinaryTrees

-- A data type to expression a relation.
-- common pattern in dependently typed programming.
data LTE : Nat -> Nat -> Type where
  LTEZero : LTE Z right -- zero is the smallest Natural.
  LTESucc : LTE left right -> LTE (S left) (S right)

data Tree : Nat -> Type where
  Leaf : (k : Nat) -> Tree k
  Branch : (l : Tree k) -> (x : Nat) -> (r : Tree j)
        -> {auto prf : LTE (S k) x} -> {auto prf2 : LTE (S x) j}
        -> Tree x

ex1 : Tree 4
ex1 = Branch (Leaf 3) 4 (Leaf 5)

ex2 : Tree 4
ex2 = ?uncomment --Branch (Leaf 5) 4 (Leaf 3)
