module TotalRoseTree
%default total
data Rose : Type -> Type where
  MkRose : a -> List (Rose a) -> Rose a
