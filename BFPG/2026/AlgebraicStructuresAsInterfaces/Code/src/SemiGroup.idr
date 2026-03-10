module SemiGroup

import Data.Nat -- for plusAssociative
import Data.List -- for appendAssociative

%default total

-- Due to time constraints I omit the following:
--  - Monads (hopefully covered in the next talk by Carlo)
--  - Free Algebra (Sorry Jack!) - it will come in a future talk.

-- Who has formally taken abstract algebra?

-- Discuss background

-- Why do we care?
--   - principled abstractions, expectations of behaviour.
--   - laws/axioms have direct relevance to computing. 
--      * associvity is about concurrency/parallelism of computaton.
--      * transitivity is about compositionality.
--      * symmetry is about equivalence of computation.
--      * all allow for optimisations and refactoring.

-- recall ordered pairs/tuples: (x, y) is a pair of x and y.
-- recall sets. {1, 2, 3} is a set. {1, 1, 2, 3} is the same set.

-- An Algerbraic Structure is often given as a triple, (S, O, A).
-- Where S is called the underlying set.
-- Where O is our set of operations.
-- Where A is our set of axioms (laws).

-- Algebraic Presentation (specification)
------------------------------------------------------------------
-- SemiGroup = (S, O, A)
-- S = {a}
-- O = {· : (a, a) → a}
-- A = {associativity : (x · y) · z = x · (y · z)}  
------------------------------------------------------------------
public export
interface SemiGroup (a : Type) where
  · : a -> a -> a
  axiomAssoc : (x, y, z : a) -> (x `·` y) `·` z = x `·` (y `·` z)

-- Concrete Realisation
------------------------------------------------------------------
-- (𝔹, ∧) is a semigroup. (axioms are implied here)
-- S = {True, False) = 𝔹
-- O = ∧  (logical and)
-- A = (x ∧ y) ∧ z = x ∧ (y ∧ z)
------------------------------------------------------------------
public export
implementation [sgAnd] SemiGroup Bool where
   x `·` y  = x && y

   axiomAssoc True  y z = Refl
   axiomAssoc False y z = Refl

-- Concrete Realisation or Model
------------------------------------------------------------------
-- (𝔹, ∨) is a semigroup. (axioms are implied here)
-- S = {True, False) = 𝔹
-- O = ∨  (logical or)
-- A = (x ∨ y) ∨ z = x ∨ (y ∨ z)
------------------------------------------------------------------
public export
implementation [sgOr] SemiGroup Bool where
   x `·` y = x || y

   axiomAssoc True  y z = Refl
   axiomAssoc False y z = Refl

-- Concrete Realisation or Model
------------------------------------------------------------------
-- (ℕ, +) is a semigroup. (axioms are implied here)
-- S = ℕ
-- O = +
-- A = (x + y) + z = x + (y + z)
------------------------------------------------------------------
public export
implementation [sgAdd] SemiGroup Nat where
  · = (+)

  axiomAssoc x y z = sym $ plusAssociative x y z

-- Concrete Realisation or Model
------------------------------------------------------------------
-- (ℕ, +) is a semigroup. (axioms are implied here)
-- S = ℕ
-- O = +
-- A = (x + y) + z = x + (y + z)
------------------------------------------------------------------
public export
implementation [sgMult] SemiGroup Nat where
  · = (*)

  axiomAssoc x y z = sym $ multAssociative x y z

-- Concrete Realisation or Model
------------------------------------------------------------------
-- (List(a), ++) is a semigroup. (axioms are implied here)
-- S = List(a) - the set of lists parameterised by a
-- O = ++
-- A = (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
------------------------------------------------------------------
public export
implementation [listConcat] {a : Type} -> SemiGroup (List a) where
  · = (++)

  axiomAssoc xs ys zs = sym $ appendAssociative xs ys zs

