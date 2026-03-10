module Monoid
import SemiGroup

import Data.Nat 
-- for plusZeroRightNeutral, multOneLeftNeutral
--     multOneRightNeutral
import Data.List
-- for appendNilRightNeutral

%hide Prelude.Monoid
%default total

-- Algebraic Presentation (specification)
------------------------------------------------------------------
-- Monoid = (S, O, A)
-- S = {a}
-- O = {· : (a, a) → a [from semigroup], ε : a}
-- A = {associativity : (x · y) · z = x · (y · z) [from semigroup]
--      left identity : ε · x = x, right identity : x · ε = x}
------------------------------------------------------------------
public export
interface SemiGroup a => Monoid (a : Type) where
  ε : a
  axiomLeftId  : (x : a) -> ε `·` x = x
  axiomRightId : (x : a) -> x `·` ε = x

-- Concrete Realisation
------------------------------------------------------------------
-- (ℕ, +) is a monoid. (axioms are implied here)
-- S = ℕ
-- O = {+ [from semigroup], 0}
-- A = {(x + y) + z = x + (y + z) [from semigroup],
--      0 + x = x, x + 0 = x}
------------------------------------------------------------------
public export
implementation [monoidAdd] Monoid Nat using sgAdd where
  ε = 0
  axiomLeftId  x = Refl
  axiomRightId x = plusZeroRightNeutral x

-- Concrete Realisation
------------------------------------------------------------------
-- (ℕ, ×) is a monoid. (axioms are implied here)
-- S = ℕ
-- O = {× [from semigroup], 1}
-- A = {(x × y) × z = x × (y × z) [from semigroup],
--      1 × x = x, x × 1 = x}
------------------------------------------------------------------
public export
implementation [monoidMult] Monoid Nat using sgMult where
  ε = 1
  axiomLeftId  x = multOneLeftNeutral x
  axiomRightId x = multOneRightNeutral x

-- Concrete Realisation
------------------------------------------------------------------
-- (ℕ, ×) is a monoid. (axioms are implied here)
-- S = ℕ
-- O = {× [from semigroup], 1}
-- A = {(x × y) × z = x × (y × z) [from semigroup],
--      1 × x = x, x × 1 = x}
------------------------------------------------------------------
public export
implementation [monoidBand] Monoid Bool using sgAnd where
  ε = True
  axiomLeftId  x     = Refl
  axiomRightId True  = Refl
  axiomRightId False = Refl

public export
implementation [monoidBor] Monoid Bool using sgOr where
  ε = False
  axiomLeftId  x     = Refl
  axiomRightId False = Refl
  axiomRightId True  = Refl

public export
implementation [monoidListConcat] {a : Type} -> Monoid (List a) 
  using listConcat where
  ε = []
  axiomLeftId  xs = Refl
  axiomRightId xs = appendNilRightNeutral xs


