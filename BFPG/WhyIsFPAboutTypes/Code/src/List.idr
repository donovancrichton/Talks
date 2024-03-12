module List
%hide Prelude.List
%default total

data List : Type -> Type where
  Nil : List a
  (::) : a -> List a -> List a
