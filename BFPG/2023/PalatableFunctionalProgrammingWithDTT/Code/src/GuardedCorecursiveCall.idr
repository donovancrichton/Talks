module GuardedCorecursiveCall
%hide Prelude.Stream.Stream
%default total
data Stream : Type -> Type where
  (::) : a -> Inf (Stream a) -> Stream a
implementation Functor Stream where
  map f (x :: xs) = f x :: map f xs

ones : Stream Nat
ones = 1 :: ones

twos : Stream Nat
twos = map (+1) ones
