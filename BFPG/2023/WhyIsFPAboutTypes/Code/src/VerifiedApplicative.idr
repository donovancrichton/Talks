module VerifiedApplicative

import VerifiedFunctor

%default total
%hide Prelude.Interfaces.(<*>)
%hide Prelude.Interfaces.pure
%hide Prelude.map
%hide Prelude.(<$>)
%hide Prelude.Ops.infixr.(<$>)
%ambiguity_depth 10

infixl 3 <*>

public export
interface VerifiedApplicative (f : Type -> Type) where
  pure : a -> f a
  (<*>) : f (a -> b) -> f a -> f b
  idLaw : (x : f a) -> pure Prelude.Basics.id <*> x = x
--  comLaw : {f : Type -> Type} -> {a, b : Type} 
--    -> {x : f a} -> {g : f (a -> b)} -> {h : f (b -> c)} 
  comLaw : {a, b, c : Type} 
    -> (x : f a) -> (g : f (a -> b)) -> (h : f (b -> c)) 
    -> (((pure (.)) <*> h) <*> g) <*> x = h <*> (g <*> x)
  homLaw : pure f <*> pure x = pure (f x)
  intLaw : (u : f (a -> b)) -> (y : a)
    -> u <*> pure y = pure (\ff => ff y) <*> u


public export
implementation VerifiedApplicative List where
  pure x = [x]

  [] <*> ys = []
  (x :: xs) <*> ys = (x <$> ys) ++ (xs <*> ys)

  idLaw [] = Refl
  idLaw (x :: xs) = cong (x ::) (idLaw xs)

  -- Courtesy of Hideyuki!
  comLaw xs gs [] = Refl
  comLaw xs gs (h :: hs') =
    let concNil : {a : Type} -> (xs : List a) ->
                  xs ++ [] = xs -- the same as Data.List.appendNilRightNeutral
        concNil [] = Refl
        concNil (x :: xs) = cong (x ::) (concNil xs) 
    in
    let concAssoc : {p : Type} -> (l1 : List p) -> (l2 : List p) -> (l3 : List p) ->
                    l1 ++ (l2 ++ l3) = (l1 ++ l2) ++ l3 -- the same as Data.List.appendAssociative
        concAssoc [] l2 l3 = Refl 
        concAssoc (x :: xs) l2 l3 = cong (x ::) (concAssoc xs l2 l3)
    in
    let concAp : {t, u : Type} -> (l1 : List (t -> u)) -> (l2 : List (t -> u)) -> (l3 : List t) ->
                 (l1 ++ l2) <*> l3 = (l1 <*> l3) ++ (l2 <*> l3)
        concAp [] l2 l3 = Refl
        concAp (x :: xs) l2 l3 = 
          let rw1 = concAp xs l2 l3 in
          let rw2 = concAssoc (x <$> l3) (xs <*> l3) (l2 <*> l3) in
          rewrite sym rw2 in
          rewrite rw1 in
          Refl
    in
    let concFmap : {a, b : Type} -> (f : a -> b) -> (l1 : List a) -> (l2 : List a) -> 
                   f <$> (l1 ++ l2) = (f <$> l1) ++ (f <$> l2) -- almost the same as Data.List.mapAppend
        concFmap f [] l2 = Refl
        concFmap f (x :: xs) l2 = cong (f x ::) (concFmap f xs l2)
    in
    let fmapFcomp : {a, b, c : Type} -> (f : b -> c) -> (g : a -> b) -> (xs : List a) ->
                    (f . g) <$> xs = f <$> (g <$> xs) -- almost the same as Data.List.mapFusion
        fmapFcomp f g [] = Refl
        fmapFcomp f g (x :: xs') = cong (f (g x) ::) (fmapFcomp f g xs')
    in
    let apFmap : {a, b, c : Type} -> (f : b -> c) -> (gs : List (a -> b)) -> (xs : List a) -> 
                 ((f .) <$> gs) <*> xs = f <$> (gs <*> xs)
        apFmap f [] xs = Refl
        apFmap f (g :: gs') xs =
          let rw1 = concFmap f (g <$> xs) (gs' <*> xs) in
          let rw2 = fmapFcomp f g xs in
          let rw3 = apFmap f gs' xs in
          rewrite rw1 in
          rewrite rw2 in
          rewrite rw3 in
          Refl
    in
    let rw1 = concNil {a = (a -> b) -> (a -> c)} ((.) <$> hs') in
    let rw2 = comLaw xs gs hs' in
    let rw3 = concAp ((h .) <$> gs) (((.) <$> hs') <*> gs) xs in
    let rw4 = apFmap h gs xs in
    rewrite sym rw2 in
    rewrite rw1 in
    rewrite rw3 in
    rewrite rw4 in
    Refl

  homLaw = Refl

  intLaw [] u = Refl
  intLaw (f :: fs) u = cong (f u ::) (intLaw fs u)
