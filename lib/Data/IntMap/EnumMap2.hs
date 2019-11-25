{-# LANGUAGE CPP #-}

module Data.IntMap.EnumMap2 where

import Data.Foldable as F (Foldable(foldMap))
import qualified Data.IntMap as M
#if MIN_VERSION_containers(0,6,0)
import qualified Data.IntMap.Internal.Debug as MD
#else
import qualified Data.IntMap as MD
#endif
import qualified Data.IntSet.EnumSet2 as S (EnumSet(..))
import Data.Semigroup as Sem
import Prelude
import qualified Prelude as L (map)

newtype EnumMap k a = EnumMap {unEnumMap :: M.IntMap a}
  deriving (Eq,Ord,Read,Show)

instance Ord k => Sem.Semigroup (EnumMap k a) where
  EnumMap x <> EnumMap y = EnumMap (x `mappend` y)

instance Ord k => Monoid (EnumMap k a) where
  mempty = EnumMap mempty
  mappend = (<>)

instance Ord k => Functor (EnumMap k) where
  fmap f (EnumMap m) = EnumMap (fmap f m)

instance Ord k => F.Foldable (EnumMap k) where
  foldMap f (EnumMap m) = F.foldMap f m

(!) :: (Enum key) => EnumMap key a -> key -> a
(!) (EnumMap m) k = (M.!) m (fromEnum k)

(\\) :: (Enum key) => EnumMap key a -> EnumMap key b -> EnumMap key a
(\\) (EnumMap m1) (EnumMap m2) = EnumMap ((M.\\) m1 m2)

null :: (Enum key) => EnumMap key a -> Bool
null (EnumMap m) = M.null m

size :: (Enum key) => EnumMap key a -> Int
size (EnumMap m) = M.size m

member :: (Enum key) => key -> EnumMap key a -> Bool
member k (EnumMap m) = M.member (fromEnum k) m

notMember :: (Enum key) => key -> EnumMap key a -> Bool
notMember k (EnumMap m) = M.notMember (fromEnum k) m

{-# INLINE lookup #-}
lookup :: (Enum key) => key -> EnumMap key a -> Maybe a
lookup k (EnumMap m) = maybe (fail "EnumMap.lookup failed") return $ M.lookup (fromEnum k) m

findWithDefault :: (Enum key) => a -> key -> EnumMap key a -> a
findWithDefault a k (EnumMap m) = M.findWithDefault a (fromEnum k) m

empty :: (Enum key) => EnumMap key a
empty = EnumMap M.empty

singleton :: (Enum key) => key -> a -> EnumMap key a
singleton k a = EnumMap (M.singleton (fromEnum k) a)

insert :: (Enum key) => key -> a -> EnumMap key a -> EnumMap key a
insert k a (EnumMap m) = EnumMap (M.insert (fromEnum k) a m)

insertWith :: (Enum key) => (a -> a -> a) -> key -> a -> EnumMap key a -> EnumMap key a
insertWith f k a (EnumMap m) = EnumMap (M.insertWith f (fromEnum k) a m)

insertWithKey :: (Enum key) => (key -> a -> a -> a) -> key -> a -> EnumMap key a -> EnumMap key a
insertWithKey f k a (EnumMap m) = EnumMap (M.insertWithKey f' (fromEnum k) a m)
  where f' b a1 a2 = f (toEnum b) a1 a2

insertLookupWithKey :: (Enum key) => (key -> a -> a -> a) -> key -> a -> EnumMap key a -> (Maybe a, EnumMap key a)
insertLookupWithKey f k a (EnumMap m) = (ma,EnumMap m')
  where (ma,m') = M.insertLookupWithKey f' (fromEnum k) a m
        f' b a1 a2 = f (toEnum b) a1 a2

delete :: (Enum key) => key -> EnumMap key a -> EnumMap key a
delete k (EnumMap m) = EnumMap (M.delete (fromEnum k) m)

adjust :: (Enum key) => (a -> a) -> key -> EnumMap key a -> EnumMap key a
adjust f k (EnumMap m) = EnumMap (M.adjust f (fromEnum k) m)

adjustWithKey :: (Enum key) => (key -> a -> a) -> key -> EnumMap key a -> EnumMap key a
adjustWithKey f k (EnumMap m) = EnumMap (M.adjustWithKey f' (fromEnum k) m)
  where f' b a = f (toEnum b) a

update :: (Enum key) => (a -> Maybe a) -> key -> EnumMap key a -> EnumMap key a
update f k (EnumMap m) = EnumMap (M.update f (fromEnum k) m)

updateWithKey :: (Enum key) => (key -> a -> Maybe a) -> key -> EnumMap key a -> EnumMap key a
updateWithKey f k (EnumMap m) = EnumMap (M.updateWithKey f' (fromEnum k) m)
  where f' b a = f (toEnum b) a

updateLookupWithKey :: (Enum key) => (key -> a -> Maybe a) -> key -> EnumMap key a -> (Maybe a, EnumMap key a)
updateLookupWithKey f k (EnumMap m) = (a,EnumMap m')
  where (a,m') = M.updateLookupWithKey f' (fromEnum k) m
        f' b a1 = f (toEnum b) a1

union :: (Enum key) => EnumMap key a -> EnumMap key a -> EnumMap key a
union (EnumMap m1) (EnumMap m2) = EnumMap (M.union m1 m2)

unionWith :: (Enum key) => (a -> a -> a) -> EnumMap key a -> EnumMap key a -> EnumMap key a
unionWith f (EnumMap m1) (EnumMap m2) = EnumMap (M.unionWith f m1 m2)

unionWithKey :: (Enum key) => (key -> a -> a -> a) -> EnumMap key a -> EnumMap key a -> EnumMap key a
unionWithKey f (EnumMap m1) (EnumMap m2) = EnumMap (M.unionWithKey f' m1 m2)
  where f' b a1 a2 = f (toEnum b) a1 a2

unions :: (Enum key) => [EnumMap key a] -> EnumMap key a
unions cs = EnumMap (M.unions (L.map unEnumMap cs))

unionsWith :: (Enum key) => (a -> a -> a) -> [EnumMap key a] -> EnumMap key a
unionsWith f cs = EnumMap (M.unionsWith f (L.map unEnumMap cs))

difference :: (Enum key) => EnumMap key a -> EnumMap key b -> EnumMap key a
difference (EnumMap m1) (EnumMap m2) = EnumMap (M.difference m1 m2)

differenceWith :: (Enum key) => (a -> b -> Maybe a) -> EnumMap key a -> EnumMap key b -> EnumMap key a
differenceWith f (EnumMap m1) (EnumMap m2) = EnumMap (M.differenceWith f m1 m2)

differenceWithKey :: (Enum key) => (key -> a -> b -> Maybe a) -> EnumMap key a -> EnumMap key b -> EnumMap key a
differenceWithKey f (EnumMap m1) (EnumMap m2) = EnumMap (M.differenceWithKey f' m1 m2)
  where f' b a1 a2 = f (toEnum b) a1 a2

intersection :: (Enum key) => EnumMap key a -> EnumMap key b -> EnumMap key a
intersection (EnumMap m1) (EnumMap m2) = EnumMap (M.intersection m1 m2)

intersectionWith :: (Enum key) => (a -> b -> a) -> EnumMap key a -> EnumMap key b -> EnumMap key a
intersectionWith f (EnumMap m1) (EnumMap m2) = EnumMap (M.intersectionWith f m1 m2)

intersectionWithKey :: (Enum key) => (key -> a -> b -> a) -> EnumMap key a -> EnumMap key b -> EnumMap key a
intersectionWithKey f (EnumMap m1) (EnumMap m2) = EnumMap (M.intersectionWithKey f' m1 m2)
  where f' b a1 a2 = f (toEnum b) a1 a2

map :: (Enum key) => (a -> b) -> EnumMap key a -> EnumMap key b
map f (EnumMap m) = EnumMap (M.map f m)

mapWithKey :: (Enum key) => (key -> a -> b) -> EnumMap key a -> EnumMap key b
mapWithKey f (EnumMap m) = EnumMap (M.mapWithKey f' m)
  where f' b a = f (toEnum b) a

mapAccum :: (Enum key) => (a -> b -> (a, c)) -> a -> EnumMap key b -> (a, EnumMap key c)
mapAccum f a (EnumMap m) = (a',EnumMap m')
  where (a',m') = M.mapAccum f a m

mapAccumWithKey :: (Enum key) => (a -> key -> b -> (a, c)) -> a -> EnumMap key b -> (a, EnumMap key c)
mapAccumWithKey f a (EnumMap m) = (a',EnumMap m')
  where (a',m') = M.mapAccumWithKey f' a m
        f' a1 b a2 = f a1 (toEnum b) a2

fold :: (Enum key) => (a -> b -> b) -> b -> EnumMap key a -> b
fold f a (EnumMap m) = M.foldr f a m

foldWithKey :: (Enum key) => (key -> a -> b -> b) -> b -> EnumMap key a -> b
foldWithKey f a (EnumMap m) = M.foldrWithKey f' a m
  where f' b a1 a2 = f (toEnum b) a1 a2

elems :: (Enum key) => EnumMap key a -> [a]
elems (EnumMap m) = M.elems m

keys :: (Enum key) => EnumMap key a -> [key]
keys (EnumMap m) = L.map toEnum (M.keys m)

-- Have to break cover until I have CharSet
keysSet :: (Enum key) => EnumMap key a -> S.EnumSet key
keysSet (EnumMap m) = S.EnumSet (M.keysSet m)

assocs :: (Enum key) => EnumMap key a -> [(key, a)]
assocs (EnumMap m) = L.map (\(b,a) -> (toEnum b,a)) (M.assocs m)

toList :: (Enum key) => EnumMap key a -> [(key, a)]
toList (EnumMap m) = L.map (\(b,a) -> (toEnum b,a)) (M.toList m)

fromList :: (Enum key) => [(key, a)] -> EnumMap key a
fromList ka = EnumMap (M.fromList (L.map (\(k,a) -> (fromEnum k,a)) ka))

fromListWith :: (Enum key) => (a -> a -> a) -> [(key, a)] -> EnumMap key a
fromListWith f ka = EnumMap (M.fromListWith f (L.map (\(k,a) -> (fromEnum k,a)) ka))

fromListWithKey :: (Enum key) => (key -> a -> a -> a) -> [(key, a)] -> EnumMap key a
fromListWithKey f ka = EnumMap (M.fromListWithKey f' (L.map (\(k,a) -> (fromEnum k,a)) ka))
  where f' b a1 a2 = f (toEnum b) a1 a2

toAscList :: (Enum key) => EnumMap key a -> [(key, a)]
toAscList (EnumMap m) = L.map (\(b,a) -> (toEnum b,a)) (M.toAscList m)

fromAscList :: (Enum key) => [(key, a)] -> EnumMap key a
fromAscList ka = EnumMap (M.fromAscList (L.map (\(k,a) -> (fromEnum k,a)) ka))

fromAscListWith :: (Enum key) => (a -> a -> a) -> [(key, a)] -> EnumMap key a
fromAscListWith f ka = EnumMap (M.fromAscListWith f (L.map (\(k,a) -> (fromEnum k,a)) ka))

fromAscListWithKey :: (Enum key) => (key -> a -> a -> a) -> [(key, a)] -> EnumMap key a
fromAscListWithKey f ka = EnumMap (M.fromAscListWithKey f' (L.map (\(k,a) -> (fromEnum k,a)) ka))
  where f' b a1 a2 = f (toEnum b) a1 a2

fromDistinctAscList :: (Enum key) => [(key, a)] -> EnumMap key a
fromDistinctAscList ka = EnumMap (M.fromDistinctAscList (L.map (\(k,a) -> (fromEnum k,a)) ka))

filter :: (Enum key) => (a -> Bool) -> EnumMap key a -> EnumMap key a
filter f (EnumMap m) = EnumMap (M.filter f m)

filterWithKey :: (Enum key) => (key -> a -> Bool) -> EnumMap key a -> EnumMap key a
filterWithKey f (EnumMap m) = EnumMap (M.filterWithKey f' m)
  where f' b a = f (toEnum b) a

partition :: (Enum key) => (a -> Bool) -> EnumMap key a -> (EnumMap key a, EnumMap key a)
partition f (EnumMap m) = (EnumMap m1', EnumMap m2')
  where (m1',m2') = M.partition f m

partitionWithKey :: (Enum key) => (key -> a -> Bool) -> EnumMap key a -> (EnumMap key a, EnumMap key a)
partitionWithKey f (EnumMap m) = (EnumMap m1', EnumMap m2')
  where (m1',m2') = M.partitionWithKey f' m
        f' b a = f (toEnum b) a

mapMaybe :: (Enum key) => (a -> Maybe b) -> EnumMap key a -> EnumMap key b
mapMaybe f (EnumMap m) = EnumMap (M.mapMaybe f m)

mapMaybeWithKey :: (Enum key) => (key -> a -> Maybe b) -> EnumMap key a -> EnumMap key b
mapMaybeWithKey f (EnumMap m) = EnumMap (M.mapMaybeWithKey f' m)
  where f' b a = f (toEnum b) a

mapEither :: (Enum key) => (a -> Either b c) -> EnumMap key a -> (EnumMap key b, EnumMap key c)
mapEither f (EnumMap m) = (EnumMap m1', EnumMap m2')
  where (m1',m2') = M.mapEither f m

mapEitherWithKey :: (Enum key) => (key -> a -> Either b c) -> EnumMap key a -> (EnumMap key b, EnumMap key c)
mapEitherWithKey f (EnumMap m) = (EnumMap m1', EnumMap m2')
  where (m1',m2') = M.mapEitherWithKey f' m
        f' b a = f (toEnum b) a

split :: (Enum key) => key -> EnumMap key a -> (EnumMap key a, EnumMap key a)
split k (EnumMap m) = (EnumMap m1', EnumMap m2')
  where (m1',m2') = M.split (fromEnum k) m

splitLookup :: (Enum key) => key -> EnumMap key a -> (EnumMap key a, Maybe a, EnumMap key a)
splitLookup k (EnumMap m) = (EnumMap m1', a, EnumMap m2')
  where (m1',a,m2') = M.splitLookup (fromEnum k) m

isSubmapOf :: (Enum key,Eq a) => EnumMap key a -> EnumMap key a -> Bool
isSubmapOf (EnumMap m1) (EnumMap m2) = M.isSubmapOf m1 m2

isSubmapOfBy :: (Enum key) => (a -> b -> Bool) -> EnumMap key a -> EnumMap key b -> Bool
isSubmapOfBy f (EnumMap m1) (EnumMap m2) = M.isSubmapOfBy f m1 m2

isProperSubmapOf :: (Enum key,Eq a) => EnumMap key a -> EnumMap key a -> Bool
isProperSubmapOf (EnumMap m1) (EnumMap m2) = M.isProperSubmapOf m1 m2

isProperSubmapOfBy :: (Enum key) => (a -> b -> Bool) -> EnumMap key a -> EnumMap key b -> Bool
isProperSubmapOfBy f (EnumMap m1) (EnumMap m2) = M.isProperSubmapOfBy f m1 m2

showTree :: (Enum key,Show a) => EnumMap key a -> String
showTree (EnumMap m) = MD.showTree m

showTreeWith :: (Enum key,Show a) => Bool -> Bool -> EnumMap key a -> String
showTreeWith b1 b2 (EnumMap m) = MD.showTreeWith b1 b2 m
