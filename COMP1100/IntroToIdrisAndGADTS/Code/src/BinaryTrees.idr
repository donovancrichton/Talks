module BinaryTrees

-- A data type to express a relation.
-- common pattern in dependently typed programming.
data LTE : Nat -> Nat -> Type where
  LTEZero : LTE Z right -- zero is the smallest Natural.
  LTESucc : LTE left right -> LTE (S left) (S right)

-- binary trees where each node is indexed by its root value.
-- this lets us enforce an ording when constructing subtrees.
data Tree : Nat -> Type where
  Leaf : (k : Nat) -> Tree k
  Branch : (l : Tree k) -> (x : Nat) -> (r : Tree j)
        -- what is going on here?
        -> {auto prf : LTE (S k) x} -> {auto prf2 : LTE (S x) j}
        -> Tree x

ex1 : Tree 4
ex1 = Branch (Leaf 3) 4 (Leaf 5)

ex2 : Tree 4
ex2 = ?uncomment --Branch (Leaf 5) 4 (Leaf 3)

-- balanced, ordered, binary trees.
data BTree : (depth : Nat) -> (rootval : Nat) -> Type where
  BLeaf : (k : Nat) -> BTree 0 k
  BBranch : (l : BTree n k) -> (x : Nat) -> (r : BTree m j)
         -> {auto prf : LTE (S k) x} -> {auto prf2 : LTE (S x) j}
         -- minus is 'monus' so we need to check both sides.
         --   yieds difference if n > m, otherwise 0.
         -> {auto prf2 : LTE (minus n m) 1} -> {auto prf4 : LTE (minus m n) 1}
         -> BTree (S (max n m)) x

ex3 : BTree 2 5
ex3 = BBranch (BBranch (BLeaf 2) 3 (BLeaf 4))
              5
              (BLeaf 7)

-- how about this one?
ex4 : BTree 2 5
ex4 = BBranch (BBranch (BLeaf 2) 3 (BLeaf 4))
              5
              (BBranch (BLeaf 6) 7 (BLeaf 8))

ex5 : BTree 3 7
ex5 = ?check
      {- BBranch (BBranch (BBranch (BLeaf 1) 3 (BLeaf 4)) 
                       5 
                      (BLeaf 6))  
              7 
              (BLeaf 10) -}
