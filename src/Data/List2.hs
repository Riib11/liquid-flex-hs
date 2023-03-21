module Data.List2 where

{-
instance Applicative [] -- Defined in ‘GHC.Base’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Functor [] -- Defined in ‘GHC.Base’
instance Monad [] -- Defined in ‘GHC.Base’
instance Monoid [a] -- Defined in ‘GHC.Base’
instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’
instance Semigroup [a] -- Defined in ‘GHC.Base’
instance Show a => Show [a] -- Defined in ‘GHC.Show’
instance MonadFail [] -- Defined in ‘Control.Monad.Fail’
instance Read a => Read [a] -- Defined in ‘GHC.Read’
instance Foldable [] -- Defined in ‘Data.Foldable’
instance Traversable [] -- Defined in ‘Data.Traversable’
-}

-- | A list with at least 2 elements
data List2 a = List2 a a [a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (List2 a) where
  List2 x1 x2 xs <> ys = List2 x1 x2 (xs <> toList ys)

fromList :: [a] -> Maybe (List2 a)
fromList (x1 : x2 : xs) = Just $ List2 x1 x2 xs
fromList _ = Nothing

toList :: List2 a -> [a]
toList (List2 x1 x2 xs) = x1 : x2 : xs

-- x :: (a -> b -> b) -> b -> f a -> b
-- x = foldr

foldr2 :: (a -> a -> b) -> (a -> b -> b) -> List2 a -> b
foldr2 f1 f2 (List2 x1 x2 xs) = foldr f2 (f1 x1 x2) xs

foldr2' :: (a -> a -> a) -> List2 a -> a
foldr2' f (List2 x1 x2 xs) = foldr f x1 (x2 : xs)

foldl2 :: (a -> a -> b) -> (b -> a -> b) -> List2 a -> b
foldl2 f1 f2 (List2 x1 x2 xs) = foldl f2 (f1 x1 x2) xs
