module TotalMapList
%hide Prelude.Functor
%hide Prelude.map
%default total
interface Functor f where
  map : (a -> b) -> f a -> f b 
implementation Functor List where
  map f [] = []
  map f (x :: xs) = f x :: map f xs
