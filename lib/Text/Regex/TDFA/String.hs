{- |
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'String' with the TDFA backend.

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
{- By Chris Kuklewicz, 2009. BSD License, see the LICENSE file. -}
module Text.Regex.TDFA.String(
  -- ** Types
  Regex
 ,MatchOffset
 ,MatchLength
 ,CompOption
 ,ExecOption
  -- ** Medium level API functions
 ,compile
 ,execute
 ,regexec
 ) where

import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchOffset,MatchLength,MatchArray)
import Text.Regex.TDFA.Common(common_error,Regex(..),CompOption,ExecOption(captureGroups))
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToRegex)

import Data.Array.IArray((!),elems,amap)
import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

err :: String -> a
err = common_error "Text.Regex.TDFA.String"

unwrap :: Either String v -> v
unwrap x = case x of Left msg -> err ("Text.Regex.TDFA.String died: "++msg)
                     Right v -> v

compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> String     -- ^ The regular expression to compile (ASCII only, no null bytes)
         -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt source =
  case parseRegex source of
    Left msg -> Left ("parseRegex for Text.Regex.TDFA.String failed:"++show msg)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts c e source = unwrap (compile c e source)
  makeRegexOptsM c e source = either fail return $ compile c e source

execute :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> Either String (Maybe MatchArray)
execute r s = Right (matchOnce r s)

regexec :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> Either String (Maybe (String, String, String, [String]))
regexec r txt = Right $
  case matchOnceText r txt of
    Just (pre, mt, post) | main:rest <- map fst (elems mt)
      -> Just (pre, main, post, rest)
    _ -> Nothing

-- Minimal definition for now
instance RegexLike Regex String where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = Tester.matchTest
  -- matchOnceText
  matchAllText r s =
    let go i _ _ | i `seq` False = undefined
        go _i _t [] = []
        go i t (x:xs) = let (off0,len0) = x!0
                            trans pair@(off,len) = (take len (drop (off-i) t),pair)
                            t' = drop (off0+len0-i) t
                        in amap trans x : seq t' (go (off0+len0) t' xs)
    in go 0 s (matchAll r s)

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM
