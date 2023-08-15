module GuardedRecursiveCall
%default total
len : List a -> Nat
len (x :: xs) = 1 + length xs
len [] = 0
