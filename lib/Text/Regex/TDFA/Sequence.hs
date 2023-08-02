{-|
This modules provides 'RegexMaker' and 'RegexLike' instances for using
@ByteString@ with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.Sequence(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Sequence(Seq)
import Data.Foldable as F(toList)

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..),Extract(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups))
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.ReadRegex(parseRegex)

import Data.Array.IArray((!),elems)
import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex (Seq Char) (Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (Seq Char) where
  makeRegexOptsM c e source =
    case parseRegex (F.toList source) of
      Left err -> fail $ "parseRegex for Text.Regex.TDFA.Sequence failed:"++show err
      Right pattern -> return $ patternToRegex pattern c e

instance RegexLike Regex (Seq Char) where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = Tester.matchTest
  matchOnceText regex source =
    fmap (\ma -> let (o,l) = ma!0
                 in (before o source
                    ,fmap (\ol -> (extract ol source,ol)) ma
                    ,after (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol -> (extract ol source,ol)))
        (matchAll regex source)

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> Seq Char   -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (F.toList bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.Sequence failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex    -- ^ Compiled regular expression
        -> Seq Char -- ^ String to match against
        -> Either String (Maybe MatchArray)
execute r bs = Right (matchOnce r bs)

regexec :: Regex    -- ^ Compiled regular expression
        -> Seq Char -- ^ String to match against
        -> Either String (Maybe (Seq Char, Seq Char, Seq Char, [Seq Char]))
regexec r txt = Right $
  case matchOnceText r txt of
    Just (pre, mt, post) | main:rest <- map fst (elems mt)
      -> Just (pre, main, post, rest)
    _ -> Nothing
