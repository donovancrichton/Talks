data MyType a = MyStr String a | MyInt Int a

data Expr a 
  = I Int
  | B Bool
  | Add (Expr Int) (Expr Int)
  | And (Expr Bool) (Expr Bool)

checkInt : Expr a -> Maybe Int
checkInt (I x) = Just x
checkInt (Add x y) = pure (+) <*> (checkInt x) <*> (checkInt y)
checkInt _     = Nothing

checkBool : Expr a -> Maybe Bool
checkBool (B x) = Just x
checkBool (And x y) = pure (&&) <*> (checkBool x) <*> (Delay (checkBool y))
checkBool _     = Nothing


main : IO ()
main = do
  let ex = And (B True) (B False)
  let check1 = checkInt ex
  if check1 == Nothing then do
    let result = checkBool ex
    print "trying bool"
    return ()
  else do
    let result = check1
    print "found int"
    return ()
  
