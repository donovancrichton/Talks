module Conjunction where
type Name = String
type Age = Integer
data Person = Person Name Age
-- Name and Age Implies Personhood
f :: (Name, Age) -> Person
f (s, k) = Person s k
