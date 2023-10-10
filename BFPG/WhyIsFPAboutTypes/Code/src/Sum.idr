module Sum
%default total

data Either : Type -> Type -> Type where
  Left : a -> Sum a b
  Right : b -> Sum a b


