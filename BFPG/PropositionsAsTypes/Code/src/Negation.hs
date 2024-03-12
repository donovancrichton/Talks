module Negation where
import Data.Void
-- It is not that case that Name and Age imply Personhood.
type Name = String
type Age = Integer
data Person = Person Name Age



