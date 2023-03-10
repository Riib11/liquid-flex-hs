{-# OPTIONS_GHC "-Wno-name-shadowing" #-}

module Utility where

import Control.Lens
import Control.Monad
import Control.Monad.Reader (MonadReader (ask, local), asks)
import Control.Monad.State (MonadState (get, put))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint.HughesPJClass (Doc)

unimplemented :: String -> a
unimplemented str = error $ "unimplemented: " <> str

backend :: String -> a
backend str = error $ "requires backend: " <> str

asUncurry :: (a' -> b' -> c) -> ((a, b) -> (a', b')) -> (a -> b -> c)
asUncurry f g = curry (uncurry f . g)

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = ((f <$>) <$>)

infixr 0 <$$>

(=<<<) :: Monad m => (a -> m b) -> m (m a) -> m b
k =<<< mma = do
  ma <- mma
  a <- ma
  k a

infixl 1 =<<<

(>>>=) :: Monad m => m (m a) -> (a -> m b) -> m b
mma >>>= k = do
  ma <- mma
  a <- ma
  k a

infixl 1 >>>=

(=<<$>) :: (Monad m, Functor f) => (a -> m b) -> f (m a) -> f (m b)
f =<<$> m = (f =<<) <$> m

infixr 0 =<<$>

($>) :: Applicative f => f a -> b -> f b
($>) = flip (<$)

infixl 4 $>

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

mapTupleM :: Monad m => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
mapTupleM fa fb = uncurry (liftM2 (,)) . bimap fa fb

isoFrom :: Iso' a b -> (b -> a)
isoFrom iso = withIso iso \_ from -> from

isoTo :: Iso' a b -> (a -> b)
isoTo iso = withIso iso const

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
secondM = bimapM pure

sequenceTuple :: Monad m => (m a, m b) -> m (a, b)
sequenceTuple (ma, mb) = (,) <$> ma <*> mb

foldrM :: (Monad m, Foldable t) => (a -> b -> m b) -> b -> t a -> m b
foldrM f b0 = foldr (\a mb -> mb >>= \b -> f a b) (pure b0)

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

-- modifyM :: Monad m => Lens' a b -> (b -> m b) -> (a -> m a)
-- modifyM l mb a = do
--   b' <- mb (a ^. l)
--   return (a & l .~ b')

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

modifyingM :: MonadState s m => Lens' s a -> (a -> m a) -> m ()
modifyingM l f = modifyM (l f)

localM :: MonadReader r m => (r -> m r) -> m a -> m a
localM f m = ask >>= f >>= flip local m . const

locallyM :: MonadReader r m => Lens' r r' -> (r' -> m r') -> m a -> m a
locallyM l f = localM (l f)

ffoldr231 :: Foldable t => b -> t a -> (a -> b -> b) -> b
ffoldr231 b ta f = foldr f b ta

ffoldr312 :: Foldable t => t a -> (a -> b -> b) -> b -> b
ffoldr312 ta f b = foldr f b ta

fromJustDefault :: a -> Maybe a -> a
fromJustDefault a = \case
  Nothing -> a
  Just a' -> a'

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
findMaybe f as = foldr (`maybe` Just) Nothing (f <$> as)

findMaybeM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findMaybeM _ [] = return Nothing
findMaybeM f (a : as) =
  f a >>= \case
    False -> findMaybeM f as
    True -> return (Just a)

applyArg1 :: (a -> a') -> (a' -> b -> c) -> (a -> b -> c)
applyArg1 f g a = g (f a)

mconcatMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mconcatMap f = foldr (applyArg1 f (<>)) mempty

mapFromListMaybeKey :: Ord k => [(Maybe k, v)] -> Map.Map k v
mapFromListMaybeKey = Map.fromList . concatMap (\(mb_txt, tm) -> maybe [] (\txt -> [(txt, tm)]) mb_txt)

mapFromKeyedList :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapFromKeyedList toKey = Map.fromList . fmap (\v -> (toKey v, v))

justs :: [Maybe a] -> [a]
justs = concatMap (maybe [] pure)

foldrM' :: (Foldable t, Monad m) => t a -> b -> (a -> b -> m b) -> m b
foldrM' ta b f = foldM (flip f) b ta

foldlM' :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
foldlM' ta b f = foldM f b ta

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

(@) :: [a] -> a -> [a]
xs @ x = xs <> [x]

comps :: [a -> a] -> a -> a
comps fs a = foldr ($) a fs

compsM :: Monad m => [a -> m a] -> a -> m a
compsM ks a = foldM (flip ($)) a ks

ticks :: Doc -> Doc
ticks doc = "`" <> doc <> "`"

angles :: Doc -> Doc
angles doc = "<" <> doc <> ">"

foldr' :: Foldable t => (a -> b -> b) -> t a -> b -> b
foldr' f ta b = foldr f b ta

for :: Foldable t => t a -> b -> (a -> b -> b) -> b
for ta b f = foldr f b ta
