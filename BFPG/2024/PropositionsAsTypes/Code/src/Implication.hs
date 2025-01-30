module Implication where
-- Strings imply Integers
f :: String -> Integer
f "zero" = 0
f "one"  = 1
f "two"  = 2
f _      = -1
