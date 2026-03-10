module Category

import Data.Nat -- for LTE

%default total

------------------------------------------------------------
-- We often see categories defined as objects '.' and 
-- morphism's between them '->' but this is a very
-- geometric definition, modern programmers encounter very
-- little abstract geometry. 

-- I propose that it is better to introduce categories to
-- software engineers algebraically instead.
------------------------------------------------------------


------------------------------------------------------------
-- Category = (S, O, A)
-- S = (Objs, Morphs)
--      Objs   = {obj1, obj2, obj3 ...} etc.
--      Morphs = {obj1 -> obj1, obj1 -> obj2, obj3 -> obj1} etc.
-- O = {id : (a : obj) -> a -> a, 
--       ∘ : (a, b, c : obj) -> (b -> c) -> (a -> b) -> (a - c)}
-- A = {left id : id ∘ g = g, right id : f ∘ id = f,
--      associativity of ∘ : (f ∘ g) ∘ h = f ∘ (g ∘ h)}

interface Category (obj : Type) (morphism : obj -> obj -> Type) 
  where
  id : {a : obj} -> morphism a a
  ∘  : {a, b, c : obj} -> morphism b c -> morphism a b -> morphism a c

  axiomCatLeftId  : {a, b : obj} -> {f : morphism a b} -> id `∘` f = f
  axiomCatRightId : {a, b : obj} -> {g : morphism a b} -> g `∘` id = g
  axiomCatAssoc   : {a, b, c, d : obj} -> {f : morphism a b} 
                 -> {g : morphism b c} -> {h : morphism c d} 
                 -> (h `∘` g) `∘` f = h `∘` (g `∘` f)

-- The type containing exactly 1 element.
-- The object 'collection' with exactly 1 object.
data Unit : Type where
  MkUnit : Unit

-- A single morphism between the elements of the single object
-- there is only one object, so it's always to and from itself.
data UnitMorphism : Category.Unit -> Category.Unit -> Type where
  MkUnitMorphism : UnitMorphism MkUnit MkUnit 

-- A trivial category.
-- (S, O, A)
-- S = (Unit, Unit -> Unit -> Type)
-- O = {id : Unit -> Unit, 
--       ∘ : {a, b, c : Unit} -> (Unit -> Unit) 
--           -> (Unit -> Unit) -> (Unit -> Unit)}
-- A = {left id  : id ∘ f = f, right id : g ∘ id = g, etc}
implementation Category Category.Unit UnitMorphism where
  id {a = MkUnit} = MkUnitMorphism
  MkUnitMorphism `∘` MkUnitMorphism = MkUnitMorphism

  axiomCatLeftId  {a = MkUnit, b = MkUnit, f = MkUnitMorphism} = Refl
  axiomCatRightId {a = MkUnit, b = MkUnit, g = MkUnitMorphism} = Refl
  axiomCatAssoc {a = MkUnit, b = MkUnit, c = MkUnit, d = MkUnit,
                 f = MkUnitMorphism, g = MkUnitMorphism, h = MkUnitMorphism} 
                 = Refl

-- A Functor is a morphism between categories. 
-- because `f` is total we transport all objects in Category A
-- to Cateogory B.
interface (Category objA morA, Category objB morB)
  => CatFunctor (objA : Type) (morA : objA -> objA -> Type)
                (objB : Type) (morB : objB -> objB -> Type)
                (f : objA -> objB) where
       catmap : {a, b : objA} -> morA a b -> morB (f a) (f b)
   
       axiomCatFunctorId : {x : objA} 
                        -> catmap (Category.id {a = x}) = Category.id {a = f x}
                        
       axiomCatFunctorCompose : {a, b, c : objA} 
                             -> {g : morA b c} -> {h : morA a b}
                             -> catmap (∘ {a = a, b = b, c = c} g h) = 
                                  ∘ {a = f a, b = f b, c = f c} (catmap g) (catmap h)

-- if (p1 : a ≤ b) and (p2 : a ≤ b) then p1 = p2
-- our definition of preorder.
lteUnique : {a, b : Nat} -> (p1, p2 : LTE a b) -> p1 = p2
lteUnique LTEZero     LTEZero     = Refl
lteUnique (LTESucc x) (LTESucc y) = cong LTESucc (lteUnique x y)

-- A preorder category over the natural numbers.
implementation Category Nat LTE where

  -- reflexive and transitive defined in Data.Nat
  id      = reflexive
  (∘) g f = transitive f g

  axiomCatLeftId           = lteUnique (transitive f reflexive) f
  axiomCatRightId          = lteUnique (transitive reflexive g) g
  axiomCatAssoc {f, g, h}  = 
    let 
      lhs : LTE a d
      lhs = transitive f (transitive g h)
      
      rhs : LTE a d
      rhs = transitive (transitive f g) h
    in lteUnique lhs rhs


