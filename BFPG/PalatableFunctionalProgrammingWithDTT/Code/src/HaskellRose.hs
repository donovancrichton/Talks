module HaskellRose where
{-# LANGUAGE GADTs #-}

data Rose :: * -> * 
  where
  MkRose :: a -> [Rose a] 
    -> Rose a

instance Functor Rose 
  where
  fmap f 
   (MkRose node children) 
      = MkRose (f node) 
        (fmap (fmap f) 
          children)
