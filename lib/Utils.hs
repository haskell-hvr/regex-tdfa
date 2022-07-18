{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | Internal module for utilities used in the implementation.

module Utils (module Utils, module X) where

import Control.Applicative (Const(..))
import Control.Applicative as X ((<*>))
import Data.Functor as X
import Data.Functor.Identity

-- * Lenses
---------------------------------------------------------------------------

type Lens' o i = forall f. Functor f => (i -> f i) -> (o -> f o)

type LensGet o i = o -> i
type LensSet o i = i -> o -> o
type LensMap o i = (i -> i) -> o -> o

infixl 8 ^.
-- | Get inner part @i@ of structure @o@ as designated by @Lens' o i@.
(^.) :: o -> Lens' o i -> i
o ^. l = getConst $ l Const o

-- | Set inner part @i@ of structure @o@ as designated by @Lens' o i@.
set :: Lens' o i -> LensSet o i
set l = over l . const

-- | Modify inner part @i@ of structure @o@ using a function @i -> i@.
over :: Lens' o i -> LensMap o i
over l f o = runIdentity $ l (Identity . f) o

-- * Misc
---------------------------------------------------------------------------

#if !MIN_VERSION_base(4,11,0)
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
#endif

-- | After 'sort' or 'sortBy' the use of 'nub' or 'nubBy' can be replaced by 'norep' or 'norepBy'.
norep :: (Eq a) => [a]->[a]
norep [] = []
norep x@[_] = x
norep (a:bs@(c:cs)) | a==c = norep (a:cs)
                    | otherwise = a:norep bs

-- | After 'sort' or 'sortBy' the use of 'nub' or 'nubBy' can be replaced by 'norep' or 'norepBy'.
norepBy :: (a -> a -> Bool) -> [a] -> [a]
norepBy _ [] = []
norepBy _ x@[_] = x
norepBy eqF (a:bs@(c:cs)) | a `eqF` c = norepBy eqF (a:cs)
                          | otherwise = a:norepBy eqF bs

mapFst :: (Functor f) => (t -> t2) -> f (t, t1) -> f (t2, t1)
mapFst f = fmap (\ (a,b) -> (f a,b))

mapSnd :: (Functor f) => (t1 -> t2) -> f (t, t1) -> f (t, t2)
mapSnd f = fmap (\ (a,b) -> (a,f b))

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ
