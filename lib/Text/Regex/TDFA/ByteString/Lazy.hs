{-|
This modules provides 'RegexMaker' and 'RegexLike' instances for using
@ByteString@ with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.ByteString.Lazy(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array.IArray((!),elems,amap)
import qualified Data.ByteString.Lazy.Char8 as L(ByteString,take,drop,unpack)

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups))

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex L.ByteString L.ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption L.ByteString where
  makeRegexOptsM c e source = makeRegexOptsM c e (L.unpack source)

instance RegexLike Regex L.ByteString where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = Tester.matchTest
  matchOnceText regex source =
    fmap (\ma ->
            let (o32,l32) = ma!0
                o = fi o32
                l = fi l32
            in (L.take o source
               ,fmap (\ol@(off32,len32) ->
                        let off = fi off32
                            len = fi len32
                        in (L.take len (L.drop off source),ol)) ma
               ,L.drop (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    let go i _ _ | i `seq` False = undefined
        go _i _t [] = []
        go i t (x:xs) =
          let (off0,len0) = x!0
              trans pair@(off32,len32) = (L.take (fi len32) (L.drop (fi (off32-i)) t),pair)
              t' = L.drop (fi (off0+len0-i)) t
          in amap trans x : seq t' (go (off0+len0) t' xs)
    in go 0 source (matchAll regex source)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> L.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (L.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.ByteString failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex        -- ^ Compiled regular expression
        -> L.ByteString -- ^ ByteString to match against
        -> Either String (Maybe MatchArray)
execute r bs = Right (matchOnce r bs)

regexec :: Regex        -- ^ Compiled regular expression
        -> L.ByteString -- ^ ByteString to match against
        -> Either String (Maybe (L.ByteString, L.ByteString, L.ByteString, [L.ByteString]))
regexec r txt = Right $
  case matchOnceText r txt of
    Just (pre, mt, post) | main:rest <- map fst (elems mt)
      -> Just (pre, main, post, rest)
    _ -> Nothing
