{-|
Module      :  Text.Regex.TDFA.Text.Lazy
Copyright   :  Chris Kuklewicz 2007-2009, shelarcy 2012
License     :  BSD-style (see the file LICENSE)

Maintainer  :  shelarcy <shelarcy@gmail.com>
Stability   :  experimental
Portability :  GHC (uses text)

This modules provides 'RegexMaker' and 'RegexLike' instances for using
'Text' with the TDFA backend ("Text.Regex.TDFA.NewDFA.Engine" and
"Text.Regex.TDFA.NewDFA.Tester").

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.Text.Lazy(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array.IArray(Array,(!),elems,amap)
import qualified Data.Text.Lazy as L(Text,empty,take,drop,uncons,unpack)

import Text.Regex.Base(MatchArray,RegexContext(..),Extract(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups),Position)

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Uncons(Uncons(uncons))
import qualified Text.Regex.TDFA.NewDFA.Engine as Engine(execMatch)
import qualified Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

instance Extract L.Text where
  before = L.take . toEnum; after = L.drop . toEnum; empty = L.empty

instance RegexContext Regex L.Text L.Text where
  match = polymatch
  matchM = polymatchM

instance Uncons L.Text where
  {- INLINE uncons #-}
  uncons = L.uncons

instance RegexMaker Regex CompOption ExecOption L.Text where
  makeRegexOptsM c e source = makeRegexOptsM c e (L.unpack source)

{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> L.Text -> [MatchArray] #-}
execMatch :: Uncons text => Regex -> Position -> Char -> text -> [MatchArray]
execMatch = Engine.execMatch

{-# SPECIALIZE myMatchTest :: Regex -> L.Text -> Bool #-}
myMatchTest :: Uncons text => Regex -> text -> Bool
myMatchTest = Tester.matchTest

instance RegexLike Regex L.Text where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = myMatchTest
  matchOnceText regex source =
    fmap (\ ma ->
            let (o,l) = ma!0
            in (before o source
               ,fmap (\ ol -> (extract ol source,ol)) ma
               ,after (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    let go :: Int -> L.Text -> [Array Int (Int, Int)] -> [Array Int (L.Text, (Int, Int))]
        go i _ _ | i `seq` False = undefined
        go _i _t [] = []
        go i t (x:xs) =
          let (off0,len0) = x!0
              trans pair@(off,len) = (extract (off-i,len) t,pair)
              t' = after (off0+(len0-i)) t
          in fmap trans x : seq t' (go (off0+len0) t' xs)
    in go 0 source (matchAll regex source)

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> L.Text -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt txt =
  case parseRegex (L.unpack txt) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.Text.Lazy failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex      -- ^ Compiled regular expression
        -> L.Text -- ^ Text to match against
        -> Either String (Maybe MatchArray)
execute r txt = Right (matchOnce r txt)

regexec :: Regex      -- ^ Compiled regular expression
        -> L.Text -- ^ Text to match against
        -> Either String (Maybe (L.Text, L.Text, L.Text, [L.Text]))
regexec r txt =
  case matchOnceText r txt of
    Nothing -> Right (Nothing)
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
