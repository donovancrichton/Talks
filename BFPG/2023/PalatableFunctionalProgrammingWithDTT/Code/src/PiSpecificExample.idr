module PiSpecificExample
data Vector : Nat -> Type -> Type where
  Nil : Vector 0 a
  (::) : a -> Vector k a -> Vector (S k) a

trues : (k : Nat) -> Vector k Bool
trues Z = []
trues (S k) = True :: trues k
