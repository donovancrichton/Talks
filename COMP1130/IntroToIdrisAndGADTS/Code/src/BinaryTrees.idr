module BinaryTrees

-- A data type to express a relation.
-- common pattern in dependently typed programming.
data LTE : Nat -> Nat -> Type where
  LTEZero : LTE Z right -- zero is the smallest Natural.
  LTESucc : LTE left right -> LTE (S left) (S right)

-- binary trees where each node is indexed by its min and max values.
-- this lets us enforce an ording when constructing subtrees.

-- NOTE: As correctly pointed out in the lecture, indexing by
--       a single value only preserves ordering at the sub-tree
--       level, not the entire tree. So we've modified the
--       definition here to preserve ordering at the entire tree
--       which requires addition an additional Nat index to the
--       type constructor of the tree.
data Tree : (min : Nat) -> (max : Nat) -> Type where
  Leaf : (k : Nat) -> Tree k k
  Branch : (l : Tree lmin lmax) -> (x : Nat) -> (r : Tree rmin rmax)
        -> {auto prf : LTE (S lmax) x} -> {auto prf2 : LTE (S x) rmin}
        -> {auto prf3 : LTE lmin lmax} -> {auto prf4 : LTE rmin rmax}
        -> Tree lmin rmax

ex1 : Tree 3 5
ex1 = Branch (Leaf 3) 4 (Leaf 5)

<<<<<<< HEAD
-- this will not type check
ex2 : Tree 3 8
ex2 = ?hole -- Branch (Leaf 3) 5 (Branch (Leaf 4) 7 (Leaf 8))



-- balanced, ordered, binary trees.
data BTree : (height : Nat) -> (min : Nat) -> (max : Nat) -> Type where
  BLeaf : (k : Nat) -> BTree 0 k k
  BBranch : (l : BTree n lmin lmax) -> (x : Nat) -> (r : BTree m rmin rmax)
         -> {auto prf : LTE (S lmax) x} -> {auto prf2 : LTE (S x) rmin}
         -> {auto prf3 : LTE lmin lmax} -> {auto prf4 : LTE rmin rmax}
=======
ex2 : Tree 4
ex2 = ?hole -- Branch (Leaf 5) 4 (Leaf 3)

example : Tree 5
example = Branch (Leaf 3) 5 (Branch (Leaf 4) 6 (Leaf 7))

-- balanced, ordered, binary trees.
data BTree : (height : Nat) -> (rootval : Nat) -> Type where
  BLeaf : (k : Nat) -> BTree 0 k
  BBranch : (l : BTree n k) -> (x : Nat) -> (r : BTree m j)
         -> {auto prf : LTE (S k) x} -> {auto prf2 : LTE (S x) j}
>>>>>>> dc86e1c (initial commit BFPG Talk - PhD Research)
         -- minus is 'monus' so we need to check both sides.
         --   yieds difference if n > m, otherwise 0.
         -> {auto prf5 : LTE (minus n m) 1} -> {auto prf6 : LTE (minus m n) 1}
         -> BTree (S (max n m)) lmin rmax

ex3 : BTree 2 2 7
ex3 = BBranch (BBranch (BLeaf 2) 3 (BLeaf 4))
              5
              (BLeaf 7)

<<<<<<< HEAD
=======
-- how about this one?
ex4 : BTree 2 5
ex4 = BBranch (BBranch (BLeaf 2) 3 (BLeaf 4))
              5
              (BBranch (BLeaf 6) 7 (BLeaf 8))

ex5 : BTree 3 7
ex5 = BBranch (BBranch (BBranch (BLeaf 1) 3 (BLeaf 4)) 
                       5 
                       (BLeaf 6))  
              7 
              (BLeaf 10)
>>>>>>> dc86e1c (initial commit BFPG Talk - PhD Research)
