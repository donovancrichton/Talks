module IntensionalEquality -- single = is prohibited.
data (==) : (a : Type) -> (b : Type) -> Type where
  Refl : a == a
