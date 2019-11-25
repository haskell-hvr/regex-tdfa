{-|
Module      :  Text.Regex.TDFA.Text
Copyright   :  Chris Kuklewicz 2007-2009, shelarcy 2012
License     :  BSD-style (see the file LICENSE)

This modules provides 'RegexMaker' and 'RegexLike' instances for using
'Text' with the TDFA backend ("Text.Regex.TDFA.NewDFA.Engine" and
"Text.Regex.TDFA.NewDFA.Tester").

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.

@since 1.3.1
-}
module Text.Regex.TDFA.Text(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array((!),elems)
import qualified Data.Text as T(Text,unpack)

import Text.Regex.Base(RegexLike(..),RegexMaker(..),Extract(..),MatchArray,RegexContext(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups),Position)

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Uncons(Uncons)
import qualified Text.Regex.TDFA.NewDFA.Engine as Engine(execMatch)
import qualified Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

-- | @since 1.3.1
instance RegexContext Regex T.Text T.Text where
  match = polymatch
  matchM = polymatchM

-- | @since 1.3.1
instance RegexMaker Regex CompOption ExecOption T.Text where
  makeRegexOptsM c e source = makeRegexOptsM c e (T.unpack source)

-- | @since 1.3.1
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> T.Text -> [MatchArray] #-}
execMatch :: Uncons text => Regex -> Position -> Char -> text -> [MatchArray]
execMatch = Engine.execMatch

-- | @since 1.3.1
{-# SPECIALIZE myMatchTest :: Regex -> T.Text -> Bool #-}
myMatchTest :: Uncons text => Regex -> text -> Bool
myMatchTest = Tester.matchTest

-- | @since 1.3.1
instance RegexLike Regex T.Text where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = myMatchTest
  matchOnceText regex source =
    fmap (\ma -> let (o,l) = ma!0
                 in (before o source
                    ,fmap (\ol -> (extract ol source,ol)) ma
                    ,after (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol -> (extract ol source,ol)))
        (matchAll regex source)

-- | @since 1.3.1
compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> T.Text -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt txt =
  case parseRegex (T.unpack txt) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.Text failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

-- | @since 1.3.1
execute :: Regex      -- ^ Compiled regular expression
        -> T.Text -- ^ Text to match against
        -> Either String (Maybe MatchArray)
execute r txt = Right (matchOnce r txt)

-- | @since 1.3.1
regexec :: Regex      -- ^ Compiled regular expression
        -> T.Text -- ^ Text to match against
        -> Either String (Maybe (T.Text, T.Text, T.Text, [T.Text]))
regexec r txt =
  case matchOnceText r txt of
    Nothing -> Right (Nothing)
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
