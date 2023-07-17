module HaskellRose where
{-# LANGUAGE GADTs #-}

data Rose :: * -> * where
  MkRose :: a -> [Rose a] -> Rose a
  deriving Show

instance Functor Rose where
  fmap f (MkRose node children) = 
    MkRose (f node) (fmap (fmap f) children)

example :: Rose Int
example = MkRose 0 [
            MkRose 1 [MkRose 0 []]
            , MkRose 1 [
                MkRose 2 [
                  MkRose 0 [], MkRose 0 [] 
                         ]
                       ]
                   ]

fmapTest :: Rose Int
fmapTest = fmap (+2) example
