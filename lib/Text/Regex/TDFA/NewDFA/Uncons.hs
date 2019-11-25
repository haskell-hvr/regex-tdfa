module Text.Regex.TDFA.NewDFA.Uncons(Uncons(uncons)) where

import qualified Data.ByteString.Char8 as SBS(ByteString,uncons)
import qualified Data.ByteString.Lazy.Char8 as LBS(ByteString,uncons)
import Data.Sequence(Seq,viewl,ViewL(EmptyL,(:<)))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class Uncons a where
  {- INLINE uncons #-}
  uncons :: a -> Maybe (Char,a)

instance Uncons ([] Char) where
  {- INLINE uncons #-}
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)

instance Uncons (Seq Char) where
  {- INLINE uncons #-}
  uncons s = case viewl s of
               EmptyL -> Nothing
               x :< xs -> Just (x,xs)

instance Uncons SBS.ByteString where
  {- INLINE uncons #-}
  uncons = SBS.uncons

instance Uncons LBS.ByteString where
  {- INLINE uncons #-}
  uncons = LBS.uncons

-- | @since 1.3.1
instance Uncons T.Text where
  {- INLINE uncons #-}
  uncons = T.uncons

-- | @since 1.3.1
instance Uncons TL.Text where
  {- INLINE uncons #-}
  uncons = TL.uncons
