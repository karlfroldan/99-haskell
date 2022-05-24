module Shared 
    ( NestedList (..)
    , Encoding (..)
    ) where 

data NestedList a = Elem  a | List [NestedList a] deriving (Eq, Show)
data Encoding a = Single a | Multiple Int a deriving (Eq, Show)

instance Functor Encoding where 
    fmap f (Single x)     = Single (f x)
    fmap f (Multiple i x) = Multiple i (f x)

-- instance Functor NestedList where
--     fmap f (Elem x)  = Elem $ f x 
--     fmap f (List xs) = List (f <$> xs)