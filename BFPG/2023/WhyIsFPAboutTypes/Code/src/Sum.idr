module Sum
%default total
%hide Prelude.Either

data Either : Type -> Type -> Type where
  Left : a -> Either a b
  Right : b -> Either a b
