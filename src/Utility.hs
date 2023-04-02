{-# OPTIONS_GHC "-Wno-name-shadowing" #-}

module Utility where

import Control.Lens
import Control.Monad
import Control.Monad.Reader (MonadReader (ask, local), asks)
import Control.Monad.State (MonadState (get, put))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Maybe as Maybe
import qualified Language.Fixpoint.Types as F
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.PrettyPrint.HughesPJClass (Doc)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

unimplemented :: String -> a
unimplemented str = error $ "unimplemented: " <> str

backend :: String -> a
backend str = error $ "requires backend: " <> str

asUncurry :: (a' -> b' -> c) -> ((a, b) -> (a', b')) -> (a -> b -> c)
asUncurry f g = curry (uncurry f . g)

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = ((f <$>) <$>)

infixr 0 <$$>

(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(<$$$>) f = (((f <$>) <$>) <$>)

infixr 0 <$$$>

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

mapTupleM :: Monad m => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
mapTupleM fa fb = uncurry (liftM2 (,)) . bimap fa fb

isoFrom :: Iso' a b -> (b -> a)
isoFrom iso = withIso iso \_ from -> from

isoTo :: Iso' a b -> (a -> b)
isoTo iso = withIso iso const

comp1 :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
comp1 f g a = f (g a)

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

foldr2 :: (a -> a -> a) -> [a] -> Maybe a
foldr2 f as = foldl2 (flip f) (reverse as)

foldl2 :: (a -> a -> a) -> [a] -> Maybe a
foldl2 _f [] = Nothing
foldl2 _f [_a] = Nothing
foldl2 f (a : a' : as) = Just $ foldl f (f a a') as

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

-- | Zips two associative lists, where the first list is considered
-- authoritative for the set of keys and order of keys. Throws error with both
-- the missing items and extra items of the second list.
zipAssoc :: Eq k => [(k, v1)] -> [(k, v2)] -> Either ([(k, v1)], [(k, v2)], [(k, v2)]) [(k, (v1, v2))]
zipAssoc kvs1 kvs2 =
  -- its ok to use `fromJust` here since we already checked that missing and
  -- extra are null
  if null missing && null extra && null dups
    then Right $ for kvs1 [] (\(k, v1) -> ((k, (v1, fromJust $ lookup k kvs2)) :))
    else Left (missing, extra, dups)
  where
    dups = duplicatesBy (\x0 x1 -> fst x0 == fst x1) kvs2
    -- iteratively remove things from kvs1 as they are found, leaving the
    -- missing that were expected to be found in kvs1
    missing = for kvs2 kvs1 \(k2, _v2) -> filter \(k1, _v1) -> k1 /= k2
    -- iteratively remove things from kvs2 as they are found, leaving the extras
    -- that were not expected to be found in kvs2
    extra = for kvs1 kvs2 \(k1, _vx1) -> filter \(k2, _v2) -> k1 /= k2

-- | The duplicates in a list, by a given comparator.
duplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
duplicatesBy _ [] = []
duplicatesBy f (x : xs) =
  let xs' = filter (not . f x) xs -- elements that are not duplicates
   in if length xs' == length xs'
        then duplicatesBy f xs -- all elements are not duplicates
        else x : duplicatesBy f xs -- some elements are not duplicates

-- \|
-- \| otherwise = duplicatesBy f xs

-- ** Pretty-Printing Utilities

pprintInline :: F.PPrint a => a -> Doc
pprintInline =
  text
    . PJ.renderStyle PJ.style {PJ.mode = PJ.OneLineMode}
    . F.pprint

-- !TODO this apparently doesn't really work... it makes the width 0
renderInline :: Doc -> String
renderInline = fullRender OneLineMode 0 0 (\_td s -> s) ""

bullet :: Doc -> Doc
bullet = ("● " <+>)

spaces :: Doc -> Doc
spaces doc = if isEmpty doc then mempty else space <> doc <> space

header :: Doc -> Doc
header doc = "══╣ " <> doc <> " ╠" <> text (replicate 40 '═')

subheader :: Doc -> Doc
subheader doc = "──┤ " <> doc <> " ├" <> text (replicate 40 '─')

commaList :: [Doc] -> Doc
commaList = hcat . punctuate (space <> comma <> space)
