module Functor

%hide Prelude.Functor
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
interface Functor (f : (a : Type) -> Type) where
  map          : (a -> b) -> f a -> f b
  axiomId      : (x : f a) -> map Prelude.Basics.id x = x
  axiomCompose : (xs : f a) -> (h : b -> c) -> (g : a -> b) 
              -> (map h . map g) xs = map (h . g) xs

-- Concrete Realisation
------------------------------------------------------------------
-- ([a], map) is a functor. (axioms are implied here)
-- S = List(a)
-- O = {map : (a -> b) -> List a -> List b} [our map on lists]
-- A = {map h . map g = map (h . g)} [point free in maths]
------------------------------------------------------------------
public export
implementation Functor List where
  map f []        = []
  map f (x :: xs) = f x :: Functor.map f xs

  axiomId []        = Refl
  axiomId (x :: xs) = rewrite axiomId xs in Refl

  axiomCompose []        h g = Refl
  axiomCompose (x :: xs) h g = rewrite axiomCompose xs h g in Refl

