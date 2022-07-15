{-# LANGUAGE CPP #-}
module Data.IntMap.CharMap2 where

#ifdef __GLASGOW_HASKELL__
import GHC.Base(unsafeChr)
#else
import Data.Char (chr)
#endif
import Data.Char as C(ord)
import Data.List as L (map)
import qualified Data.IntMap as M
#if MIN_VERSION_containers(0,5,11)
import qualified Data.IntMap.Internal.Debug as MD
#else
import qualified Data.IntMap as MD
#endif
import qualified Data.IntSet as S(IntSet)
import Data.Semigroup as Sem

#ifndef __GLASGOW_HASKELL__
unsafeChr = chr
#endif

newtype CharMap a = CharMap {unCharMap :: M.IntMap a} deriving (Eq,Ord,Read,Show)

instance Sem.Semigroup (CharMap a) where
  CharMap x <> CharMap y = CharMap (x `mappend` y)

instance Monoid (CharMap a) where
  mempty = CharMap mempty
  mappend = (<>)

instance Functor CharMap where
  fmap f (CharMap m) = CharMap (fmap f m)

type Key = Char

(!) :: CharMap a -> Key -> a
(!) (CharMap m) k = (M.!) m (C.ord k)

(\\) :: CharMap a -> CharMap b -> CharMap a
(\\) (CharMap m1) (CharMap m2) = CharMap ((M.\\) m1 m2)

null :: CharMap a -> Bool
null (CharMap m) = M.null m

size :: CharMap a -> Int
size (CharMap m) = M.size m

member :: Key -> CharMap a -> Bool
member k (CharMap m) = M.member (C.ord k) m

notMember :: Key -> CharMap a -> Bool
notMember k (CharMap m) = M.notMember (C.ord k) m

lookup :: Key -> CharMap a -> Maybe a
lookup k (CharMap m) = M.lookup (C.ord k) m

findWithDefault :: a -> Key -> CharMap a -> a
findWithDefault a k (CharMap m) = M.findWithDefault a (C.ord k) m

empty :: CharMap a
empty = CharMap M.empty

singleton :: Key -> a -> CharMap a
singleton k a = CharMap (M.singleton (C.ord k) a)

insert :: Key -> a -> CharMap a -> CharMap a
insert k a (CharMap m) = CharMap (M.insert (C.ord k) a m)

insertWith :: (a -> a -> a) -> Key -> a -> CharMap a -> CharMap a
insertWith f k a (CharMap m) = CharMap (M.insertWith f (C.ord k) a m)

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> CharMap a -> CharMap a
insertWithKey f k a (CharMap m) = CharMap (M.insertWithKey f' (C.ord k) a m)
  where f' b a1 a2 = f (unsafeChr b) a1 a2

insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> CharMap a -> (Maybe a, CharMap a)
insertLookupWithKey f k a (CharMap m) = (ma,CharMap m')
  where (ma,m') = M.insertLookupWithKey f' (C.ord k) a m
        f' b a1 a2 = f (unsafeChr b) a1 a2

delete :: Key -> CharMap a -> CharMap a
delete k (CharMap m) = CharMap (M.delete (C.ord k) m)

adjust :: (a -> a) -> Key -> CharMap a -> CharMap a
adjust f k (CharMap m) = CharMap (M.adjust f (C.ord k) m)

adjustWithKey :: (Key -> a -> a) -> Key -> CharMap a -> CharMap a
adjustWithKey f k (CharMap m) = CharMap (M.adjustWithKey f' (C.ord k) m)
  where f' b a = f (unsafeChr b) a

update :: (a -> Maybe a) -> Key -> CharMap a -> CharMap a
update f k (CharMap m) = CharMap (M.update f (C.ord k) m)

updateWithKey :: (Key -> a -> Maybe a) -> Key -> CharMap a -> CharMap a
updateWithKey f k (CharMap m) = CharMap (M.updateWithKey f' (C.ord k) m)
  where f' b a = f (unsafeChr b) a

updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> CharMap a -> (Maybe a, CharMap a)
updateLookupWithKey f k (CharMap m) = (a,CharMap m')
  where (a,m') = M.updateLookupWithKey f' (C.ord k) m
        f' b a1 = f (unsafeChr b) a1

union :: CharMap a -> CharMap a -> CharMap a
union (CharMap m1) (CharMap m2) = CharMap (M.union m1 m2)

unionWith :: (a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
unionWith f (CharMap m1) (CharMap m2) = CharMap (M.unionWith f m1 m2)

unionWithKey :: (Key -> a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
unionWithKey f (CharMap m1) (CharMap m2) = CharMap (M.unionWithKey f' m1 m2)
  where f' b a1 a2 = f (unsafeChr b) a1 a2

unions :: [CharMap a] -> CharMap a
unions cs = CharMap (M.unions (L.map unCharMap cs))

unionsWith :: (a -> a -> a) -> [CharMap a] -> CharMap a
unionsWith f cs = CharMap (M.unionsWith f (L.map unCharMap cs))

difference :: CharMap a -> CharMap b -> CharMap a
difference (CharMap m1) (CharMap m2) = CharMap (M.difference m1 m2)

differenceWith :: (a -> b -> Maybe a) -> CharMap a -> CharMap b -> CharMap a
differenceWith f (CharMap m1) (CharMap m2) = CharMap (M.differenceWith f m1 m2)

differenceWithKey :: (Key -> a -> b -> Maybe a) -> CharMap a -> CharMap b -> CharMap a
differenceWithKey f (CharMap m1) (CharMap m2) = CharMap (M.differenceWithKey f' m1 m2)
  where f' b a1 a2 = f (unsafeChr b) a1 a2

intersection :: CharMap a -> CharMap b -> CharMap a
intersection (CharMap m1) (CharMap m2) = CharMap (M.intersection m1 m2)

intersectionWith :: (a -> b -> a) -> CharMap a -> CharMap b -> CharMap a
intersectionWith f (CharMap m1) (CharMap m2) = CharMap (M.intersectionWith f m1 m2)

intersectionWithKey :: (Key -> a -> b -> a) -> CharMap a -> CharMap b -> CharMap a
intersectionWithKey f (CharMap m1) (CharMap m2) = CharMap (M.intersectionWithKey f' m1 m2)
  where f' b a1 a2 = f (unsafeChr b) a1 a2

map :: (a -> b) -> CharMap a -> CharMap b
map f (CharMap m) = CharMap (M.map f m)

mapWithKey :: (Key -> a -> b) -> CharMap a -> CharMap b
mapWithKey f (CharMap m) = CharMap (M.mapWithKey f' m)
  where f' b a = f (unsafeChr b) a

mapAccum :: (a -> b -> (a, c)) -> a -> CharMap b -> (a, CharMap c)
mapAccum f a (CharMap m) = (a',CharMap m')
  where (a',m') = M.mapAccum f a m

mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> CharMap b -> (a, CharMap c)
mapAccumWithKey f a (CharMap m) = (a',CharMap m')
  where (a',m') = M.mapAccumWithKey f' a m
        f' a1 b a2 = f a1 (unsafeChr b) a2

fold :: (a -> b -> b) -> b -> CharMap a -> b
fold f a (CharMap m) = M.foldr f a m

foldWithKey :: (Key -> a -> b -> b) -> b -> CharMap a -> b
foldWithKey f a (CharMap m) = M.foldrWithKey f' a m
  where f' b a1 a2 = f (unsafeChr b) a1 a2

elems :: CharMap a -> [a]
elems (CharMap m) = M.elems m

keys :: CharMap a -> [Key]
keys (CharMap m) = L.map unsafeChr (M.keys m)

keysSet :: CharMap a -> S.IntSet
keysSet (CharMap m) = M.keysSet m

assocs :: CharMap a -> [(Key, a)]
assocs (CharMap m) = L.map (\(b,a) -> (unsafeChr b,a)) (M.assocs m)

toList :: CharMap a -> [(Key, a)]
toList (CharMap m) = L.map (\(b,a) -> (unsafeChr b,a)) (M.toList m)

fromList :: [(Key, a)] -> CharMap a
fromList ka = CharMap (M.fromList (L.map (\(k,a) -> (C.ord k,a)) ka))

fromListWith :: (a -> a -> a) -> [(Key, a)] -> CharMap a
fromListWith f ka = CharMap (M.fromListWith f (L.map (\(k,a) -> (C.ord k,a)) ka))

fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> CharMap a
fromListWithKey f ka = CharMap (M.fromListWithKey f' (L.map (\(k,a) -> (C.ord k,a)) ka))
  where f' b a1 a2 = f (unsafeChr b) a1 a2

toAscList :: CharMap a -> [(Key, a)]
toAscList (CharMap m) = L.map (\(b,a) -> (unsafeChr b,a)) (M.toAscList m)

fromAscList :: [(Key, a)] -> CharMap a
fromAscList ka = CharMap (M.fromAscList (L.map (\(k,a) -> (C.ord k,a)) ka))

fromAscListWith :: (a -> a -> a) -> [(Key, a)] -> CharMap a
fromAscListWith f ka = CharMap (M.fromAscListWith f (L.map (\(k,a) -> (C.ord k,a)) ka))

fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> CharMap a
fromAscListWithKey f ka = CharMap (M.fromAscListWithKey f' (L.map (\(k,a) -> (C.ord k,a)) ka))
  where f' b a1 a2 = f (unsafeChr b) a1 a2

fromDistinctAscList :: [(Key, a)] -> CharMap a
fromDistinctAscList ka = CharMap (M.fromDistinctAscList (L.map (\(k,a) -> (C.ord k,a)) ka))

filter :: (a -> Bool) -> CharMap a -> CharMap a
filter f (CharMap m) = CharMap (M.filter f m)

filterWithKey :: (Key -> a -> Bool) -> CharMap a -> CharMap a
filterWithKey f (CharMap m) = CharMap (M.filterWithKey f' m)
  where f' b a = f (unsafeChr b) a

partition :: (a -> Bool) -> CharMap a -> (CharMap a, CharMap a)
partition f (CharMap m) = (CharMap m1', CharMap m2')
  where (m1',m2') = M.partition f m

partitionWithKey :: (Key -> a -> Bool) -> CharMap a -> (CharMap a, CharMap a)
partitionWithKey f (CharMap m) = (CharMap m1', CharMap m2')
  where (m1',m2') = M.partitionWithKey f' m
        f' b a = f (unsafeChr b) a

mapMaybe :: (a -> Maybe b) -> CharMap a -> CharMap b
mapMaybe f (CharMap m) = CharMap (M.mapMaybe f m)

mapMaybeWithKey :: (Key -> a -> Maybe b) -> CharMap a -> CharMap b
mapMaybeWithKey f (CharMap m) = CharMap (M.mapMaybeWithKey f' m)
  where f' b a = f (unsafeChr b) a

mapEither :: (a -> Either b c) -> CharMap a -> (CharMap b, CharMap c)
mapEither f (CharMap m) = (CharMap m1', CharMap m2')
  where (m1',m2') = M.mapEither f m

mapEitherWithKey :: (Key -> a -> Either b c) -> CharMap a -> (CharMap b, CharMap c)
mapEitherWithKey f (CharMap m) = (CharMap m1', CharMap m2')
  where (m1',m2') = M.mapEitherWithKey f' m
        f' b a = f (unsafeChr b) a

split :: Key -> CharMap a -> (CharMap a, CharMap a)
split k (CharMap m) = (CharMap m1', CharMap m2')
  where (m1',m2') = M.split (C.ord k) m

splitLookup :: Key -> CharMap a -> (CharMap a, Maybe a, CharMap a)
splitLookup k (CharMap m) = (CharMap m1', a, CharMap m2')
  where (m1',a,m2') = M.splitLookup (C.ord k) m

isSubmapOf :: Eq a => CharMap a -> CharMap a -> Bool
isSubmapOf (CharMap m1) (CharMap m2) = M.isSubmapOf m1 m2

isSubmapOfBy :: (a -> b -> Bool) -> CharMap a -> CharMap b -> Bool
isSubmapOfBy f (CharMap m1) (CharMap m2) = M.isSubmapOfBy f m1 m2

isProperSubmapOf :: Eq a => CharMap a -> CharMap a -> Bool
isProperSubmapOf (CharMap m1) (CharMap m2) = M.isProperSubmapOf m1 m2

isProperSubmapOfBy :: (a -> b -> Bool) -> CharMap a -> CharMap b -> Bool
isProperSubmapOfBy f (CharMap m1) (CharMap m2) = M.isProperSubmapOfBy f m1 m2

showTree :: Show a => CharMap a -> String
showTree (CharMap m) = MD.showTree m

showTreeWith :: Show a => Bool -> Bool -> CharMap a -> String
showTreeWith b1 b2 (CharMap m) = MD.showTreeWith b1 b2 m
{-# INLINE (!) #-}
{-# INLINE (\\) #-}
{-# INLINE null #-}
{-# INLINE size #-}
{-# INLINE member #-}
{-# INLINE notMember #-}
{-# INLINE lookup #-}
{-# INLINE findWithDefault #-}
{-# INLINE empty #-}
{-# INLINE singleton #-}
{-# INLINE insert #-}
{-# INLINE insertWith #-}
{-# INLINE insertWithKey #-}
{-# INLINE insertLookupWithKey #-}
{-# INLINE delete #-}
{-# INLINE adjust #-}
{-# INLINE adjustWithKey #-}
{-# INLINE update #-}
{-# INLINE updateWithKey #-}
{-# INLINE updateLookupWithKey #-}
{-# INLINE union #-}
{-# INLINE unionWith #-}
{-# INLINE unionWithKey #-}
{-# INLINE unions #-}
{-# INLINE unionsWith #-}
{-# INLINE difference #-}
{-# INLINE differenceWith #-}
{-# INLINE differenceWithKey #-}
{-# INLINE intersection #-}
{-# INLINE intersectionWith #-}
{-# INLINE intersectionWithKey #-}
{-# INLINE map #-}
{-# INLINE mapWithKey #-}
{-# INLINE mapAccum #-}
{-# INLINE mapAccumWithKey #-}
{-# INLINE fold #-}
{-# INLINE foldWithKey #-}
{-# INLINE elems #-}
{-# INLINE keys #-}
{-# INLINE keysSet #-}
{-# INLINE assocs #-}
{-# INLINE toList #-}
{-# INLINE fromList #-}
{-# INLINE fromListWith #-}
{-# INLINE fromListWithKey #-}
{-# INLINE toAscList #-}
{-# INLINE fromAscList #-}
{-# INLINE fromAscListWith #-}
{-# INLINE fromAscListWithKey #-}
{-# INLINE fromDistinctAscList #-}
{-# INLINE filter #-}
{-# INLINE filterWithKey #-}
{-# INLINE partition #-}
{-# INLINE partitionWithKey #-}
{-# INLINE mapMaybe #-}
{-# INLINE mapMaybeWithKey #-}
{-# INLINE mapEither #-}
{-# INLINE mapEitherWithKey #-}
{-# INLINE split #-}
{-# INLINE splitLookup #-}
{-# INLINE isSubmapOf #-}
{-# INLINE isSubmapOfBy #-}
{-# INLINE isProperSubmapOf #-}
{-# INLINE isProperSubmapOfBy #-}
{-# INLINE showTree #-}
{-# INLINE showTreeWith #-}
