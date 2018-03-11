data Expr a
  = I Int
  | B Bool
  | Add (Expr Int) (Expr Int)
  | And (Expr Bool) (Expr Bool)

--eval int
f :: Expr a -> Maybe Int
f (I x)     = Just x
f (Add x y) = pure (+) <*> f x <*> f y
f _         = Nothing

g :: Expr a -> Maybe Bool
g (B x)     = Just x
g (And x y) = pure (&&) <*> g x <*> g y
g _         = Nothing


doSomething :: Expr a -> IO ()
doSomething e = do
  let x' = if (f e) == Nothing then
    (if (g e) == Nothing) then
      Nothing else print 
  
  
