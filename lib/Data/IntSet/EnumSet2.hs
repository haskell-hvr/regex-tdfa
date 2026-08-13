module Data.IntSet.EnumSet2 where

import qualified Data.IntSet as S
import qualified Data.List as L (map)
import Data.Semigroup as Sem

newtype EnumSet e = EnumSet {unEnumSet :: S.IntSet}
  deriving (Eq,Ord,Read,Show)

instance Sem.Semigroup (EnumSet e) where
  EnumSet x <> EnumSet y = EnumSet (x `mappend` y)

instance Monoid (EnumSet e) where
  mempty = EnumSet mempty
  mappend = (<>)

(\\) :: (Enum e) => EnumSet e -> EnumSet e -> EnumSet e
(\\) (EnumSet s1) (EnumSet s2) = EnumSet ((S.\\) s1 s2)

null :: (Enum e) => EnumSet e -> Bool
null (EnumSet s) = S.null s

size :: (Enum e) => EnumSet e -> Int
size (EnumSet s) = S.size s

member :: (Enum e) => e -> EnumSet e -> Bool
member e (EnumSet s) = S.member (fromEnum e) s

notMember :: (Enum e) => Int -> EnumSet e -> Bool
notMember e (EnumSet s) = S.notMember (fromEnum e) s

isSubsetOf :: (Enum e) => EnumSet e -> EnumSet e -> Bool
isSubsetOf (EnumSet e1) (EnumSet e2) = S.isSubsetOf e1 e2

isProperSubsetOf :: (Enum e) => EnumSet e -> EnumSet e -> Bool
isProperSubsetOf (EnumSet e1) (EnumSet e2) = S.isProperSubsetOf e1 e2

empty :: (Enum e) => EnumSet e
empty = EnumSet (S.empty)

singleton :: (Enum e) => e -> EnumSet e
singleton e = EnumSet (S.singleton (fromEnum e))

insert :: (Enum e) => e -> EnumSet e -> EnumSet e
insert e (EnumSet s) = EnumSet (S.insert (fromEnum e) s)

delete :: (Enum e) => e -> EnumSet e -> EnumSet e
delete e (EnumSet s) = EnumSet (S.delete (fromEnum e) s)

union :: (Enum e) => EnumSet e -> EnumSet e -> EnumSet e
union (EnumSet s1) (EnumSet s2) = EnumSet (S.union s1 s2)

unions :: (Enum e) => [EnumSet e] -> EnumSet e
unions es = EnumSet (S.unions (L.map unEnumSet es))

difference :: (Enum e) => EnumSet e -> EnumSet e -> EnumSet e
difference (EnumSet e1) (EnumSet e2) = EnumSet (S.difference e1 e2)

intersection :: (Enum e) => EnumSet e -> EnumSet e -> EnumSet e
intersection (EnumSet e1) (EnumSet e2) = EnumSet (S.intersection e1 e2)

filter :: (Enum e) => (e -> Bool) -> EnumSet e -> EnumSet e
filter f (EnumSet s) = EnumSet (S.filter f' s)
  where f' b = f (toEnum b)

partition :: (Enum e) => (e -> Bool) -> EnumSet e -> (EnumSet e, EnumSet e)
partition f (EnumSet s) = (EnumSet s1', EnumSet s2')
  where (s1',s2') = S.partition f' s
        f' b = f (toEnum b)

split :: (Enum e) => e -> EnumSet e -> (EnumSet e, EnumSet e)
split e (EnumSet s) = (EnumSet s1', EnumSet s2')
  where (s1',s2') = S.split (fromEnum e) s

splitMember :: (Enum e) => e -> EnumSet e -> (EnumSet e, Bool, EnumSet e)
splitMember e (EnumSet s) = (EnumSet s1',a,EnumSet s2')
  where (s1',a,s2') = S.splitMember (fromEnum e) s

map :: (Enum e) => (e -> e) -> EnumSet e -> EnumSet e
map f (EnumSet s) = EnumSet (S.map f' s)
  where f' b = fromEnum (f (toEnum b))

fold :: (Enum e) => (e -> b -> b) -> b -> EnumSet e -> b
fold f a (EnumSet s) = S.foldr f' a s
  where f' b a1 = f (toEnum b) a1

elems :: (Enum e) => EnumSet e -> [e]
elems (EnumSet s) = L.map toEnum (S.elems s)

toList :: (Enum e) => EnumSet e -> [e]
toList (EnumSet s) = L.map toEnum (S.toList s)

fromList :: (Enum e) => [e] -> EnumSet e
fromList es = EnumSet (S.fromList (L.map fromEnum es))

toAscList :: (Enum e) => EnumSet e -> [e]
toAscList (EnumSet s) = L.map toEnum (S.toAscList s)

fromAscList :: (Enum e) => [e] -> EnumSet e
fromAscList es = EnumSet (S.fromAscList (L.map fromEnum es))

fromDistinctAscList :: (Enum e) => [e] -> EnumSet e
fromDistinctAscList es = EnumSet (S.fromDistinctAscList (L.map fromEnum es))

showTree :: (Enum e) => EnumSet e -> String
showTree (EnumSet s) = S.showTree s

showTreeWith :: (Enum e) => Bool -> Bool -> EnumSet e -> String
showTreeWith a1 a2 (EnumSet s) = S.showTreeWith a1 a2 s
