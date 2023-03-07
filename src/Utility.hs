module Utility where

import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

unimplemented :: String -> a
unimplemented str = error $ "unimplemented: " <> str

backend :: String -> a
backend str = error $ "requires backend: " <> str

asUncurry :: (a' -> b' -> c) -> ((a, b) -> (a', b')) -> (a -> b -> c)
asUncurry f g = curry (uncurry f . g)

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = ((f <$>) <$>)

infixr 0 <$$>

($>) :: Applicative f => f a -> b -> f b
_ $> b = pure b

infixl 4 $>

mapTupleM :: Monad m => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
mapTupleM fa fb = uncurry (liftM2 (,)) . bimap fa fb

isoFrom :: Iso' a b -> (b -> a)
isoFrom iso = withIso iso \_ from -> from

isoTo :: Iso' a b -> (a -> b)
isoTo iso = withIso iso \to _ -> to

comp2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
comp2 f g a b = f (g a b)

comp3 :: (b1 -> b2) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2)
comp3 f g x1 x2 x3 = f (g x1 x2 x3)

infixr 9 `comp2`

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = \case
  [] -> Nothing
  a : as -> go [] a as
  where
    go as' a' = \case
      [] -> Just (reverse as', a')
      a'' : as'' -> go (a' : as') a'' as''

eqF :: Eq b => (a -> b) -> a -> a -> Bool
eqF f a1 a2 = f a1 == f a2

bimapM :: Monad m => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
bimapM fa fb (a, b) = do
  (,) <$> fa a <*> fb b

firstM :: Monad m => (a -> m a') -> (a, b) -> m (a', b)
firstM f = bimapM f pure

secondM :: Monad m => (b -> m b') -> (a, b) -> m (a, b')
secondM f = bimapM pure f

sequenceTuple :: Monad m => (m a, m b) -> m (a, b)
sequenceTuple (ma, mb) = (,) <$> ma <*> mb

foldrM :: (Monad m, Foldable t) => (a -> b -> m b) -> b -> t a -> m b
foldrM f b0 ta = foldr (\a mb -> mb >>= \b -> (f a b)) (pure b0) ta

liftM2' :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftM2' k ma mb = do
  a <- ma
  b <- mb
  k a b

-- asksM :: MonadReader r m => (r -> m a) -> m a
-- asksM k = k =<< ask

setsM :: Monad m => Lens' st a -> m a -> (st -> m st)
setsM l ma st = do
  a' <- ma
  return (st & l .~ a')

modifyingM :: Monad m => Lens' a b -> (b -> m b) -> (a -> m a)
modifyingM l mb a = do
  b' <- mb (a ^. l)
  return (a & l .~ b')

_Just' :: String -> Lens' (Maybe a) a
_Just' msg = lens (maybe (error ("_Just': " <> msg)) id) (\_ a -> Just a)

-- st <- get
-- a' <- f (st ^. l)
-- put (st & l .~ a')

-- foldr123 :: (a -> b -> b) -> b -> t a -> b

ffoldr231 :: Foldable t => b -> t a -> (a -> b -> b) -> b
ffoldr231 b ta f = foldr f b ta

ffoldr312 :: Foldable t => t a -> (a -> b -> b) -> b -> b
ffoldr312 ta f b = foldr f b ta

fromJustDefault :: a -> Maybe a -> a
fromJustDefault a = \case
  Nothing -> a
  Just a' -> a'

fromJust' :: String -> Maybe a -> a
fromJust' str = \case
  Nothing -> error $ "fromJust: " <> str
  Just a -> a

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (a : as) =
  f a >>= \case
    False -> findM f as
    True -> return (Just a)

findMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMapM _ [] = return Nothing
findMapM f (a : as) =
  f a >>= \case
    Nothing -> findMapM f as
    Just b -> return (Just b)

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f as = foldr (\m1 m2 -> maybe m1 Just m2) Nothing (f <$> as)

findMaybeM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findMaybeM _ [] = return Nothing
findMaybeM f (a : as) =
  f a >>= \case
    False -> findMaybeM f as
    True -> return (Just a)

applyArg1 :: (a -> a') -> (a' -> b -> c) -> (a -> b -> c)
applyArg1 f g a b = g (f a) b

mconcatMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mconcatMap f as = foldr (applyArg1 f (<>)) mempty as

mapFromListMaybeKey :: Ord k => [(Maybe k, v)] -> Map.Map k v
mapFromListMaybeKey = Map.fromList . concatMap (\(mb_txt, tm) -> maybe [] (\txt -> [(txt, tm)]) mb_txt)

mapFromKeyedList :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapFromKeyedList toKey = Map.fromList . fmap (\v -> (toKey v, v))

justs :: [Maybe a] -> [a]
justs = concatMap (maybe [] pure)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb m =
  mb >>= \case
    False -> m
    True -> return ()

mapAsList :: Ord k' => ((k, v) -> (k', v')) -> Map.Map k v -> Map.Map k' v'
mapAsList f m = Map.fromList $ map f $ Map.toList m

mapAsListM :: (Monad m, Ord k') => ((k, v) -> m (k', v')) -> Map.Map k v -> m (Map.Map k' v')
mapAsListM f m = Map.fromList <$> mapM f (Map.toList m)

indent :: String -> String
indent = unlines . indentLines . lines

indentLines :: [String] -> [String]
indentLines = map ("  " <>)
