module Involutive
%default total

interface Involutive (a : Type) where
  involute : a -> a
  involutionHolds : (x : a) -> involute (involute x) = x

implementation Involutive Bool where
  involute = not
  involutionHolds True = Refl
  involutionHolds False = Refl

rev : List a -> List a
rev = 

implementation {a : Type} -> Involutive (List a) where
  involute = rev
  involutionHolds [] = Refl
  involutionHolds (x :: xs) = ?look

