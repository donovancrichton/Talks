module Disjunction where
type Name = String
type Age = Integer
type Error = String
data Person = Person Name Age
-- Name and Age implies Personhood or Errors.
f :: (Name, Age) -> Either Person Error
f (s, k) = 
  case (s == "" || k < 1) of
    True -> Right "Error"
    False -> Left (Person s k)
