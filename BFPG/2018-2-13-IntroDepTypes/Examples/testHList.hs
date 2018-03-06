{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall #-}


data List l where
  Nil :: List '[]
  Cons :: a -> List l -> List (a ': l)

type family Append (as :: [k]) (bs :: [k]) :: [k] where
  Append '[] bs       = bs
  Append (a ': as) bs =  a ': Append as bs

type family Tail (as :: [k]) :: [k] where
  Tail '[]       = '[]
  Tail (a ': as) = as

type family Head (as :: [k]) :: k where
  Head '[] = []
  Head (x ': xs) = x

head' :: List a -> Head a
head' (Cons x xs) = x

tail' :: List a -> List (Tail a)
tail' Nil         = Nil
tail' (Cons x xs) = xs

