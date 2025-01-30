module Product
%default total
%hide Prelude.Pair

data Pair : Type -> Type -> Type where
  MkPair : a -> b -> Pair a b
