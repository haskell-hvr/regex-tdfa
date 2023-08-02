{-|
This modules provides 'RegexMaker' and 'RegexLike' instances for using
@ByteString@ with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
{- By Chris Kuklewicz, 2009. BSD License, see the LICENSE file. -}
module Text.Regex.TDFA.ByteString(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array((!),elems)
import qualified Data.ByteString.Char8 as B(ByteString,take,drop,unpack)

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups))

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

instance RegexContext Regex B.ByteString B.ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption B.ByteString where
  makeRegexOptsM c e source = makeRegexOptsM c e (B.unpack source)

instance RegexLike Regex B.ByteString where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = Tester.matchTest
  matchOnceText regex source =
    fmap (\ma -> let (o,l) = ma!0
                 in (B.take o source
                    ,fmap (\ol@(off,len) -> (B.take len (B.drop off source),ol)) ma
                    ,B.drop (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol@(off,len) -> (B.take len (B.drop off source),ol)))
        (matchAll regex source)

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> B.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (B.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.ByteString failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex        -- ^ Compiled regular expression
        -> B.ByteString -- ^ ByteString to match against
        -> Either String (Maybe MatchArray)
execute r bs = Right (matchOnce r bs)

regexec :: Regex        -- ^ Compiled regular expression
        -> B.ByteString -- ^ ByteString to match against
        -> Either String (Maybe (B.ByteString, B.ByteString, B.ByteString, [B.ByteString]))
regexec r txt = Right $
  case matchOnceText r txt of
    Just (pre, mt, post) | main:rest <- map fst (elems mt)
      -> Just (pre, main, post, rest)
    _ -> Nothing
