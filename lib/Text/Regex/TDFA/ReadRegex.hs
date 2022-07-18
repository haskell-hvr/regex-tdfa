-- | This is a POSIX version of parseRegex that allows NUL characters.
-- Lazy\/Possessive\/Backrefs are not recognized.  Anchors \^ and \$ are
-- recognized.
--
-- A 'PGroup' returned always has @(Maybe 'GroupIndex')@ set to @(Just _)@
-- and never to @Nothing@.

module Text.Regex.TDFA.ReadRegex (parseRegex) where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Text.Regex.TDFA.Pattern {- all -}
import Text.ParserCombinators.Parsec((<|>), (<?>),
  try, runParser, many, getState, setState, CharParser, ParseError,
  sepBy1, option, notFollowedBy, many1, lookAhead, eof, between,
  string, noneOf, digit, char, anyChar)

import Control.Monad (liftM, guard)

import Data.Foldable (asum)
import qualified Data.Set as Set(fromList)

-- | An element inside @[...]@, denoting a character class.
data BracketElement
  = BEChar  Char       -- ^ A single character.
  | BERange Char Char  -- ^ A character range (e.g. @a-z@).
  | BEColl  String     -- ^ @foo@ in @[.foo.]@.
  | BEEquiv String     -- ^ @bar@ in @[=bar=]@.
  | BEClass String     -- ^ A POSIX character class (candidate), e.g. @alpha@ parsed from @[:alpha:]@.

-- | Return either an error message or a tuple of the Pattern and the
-- largest group index and the largest DoPa index (both have smallest
-- index of 1).  Since the regular expression is supplied as [Char] it
-- automatically supports unicode and @\\NUL@ characters.
parseRegex :: String -> Either ParseError (Pattern,(GroupIndex,DoPa))
parseRegex x = runParser (do pat <- p_regex
                             eof
                             (lastGroupIndex,lastDopa) <- getState
                             return (pat,(lastGroupIndex,DoPa lastDopa))) (0,0) x x

type P = CharParser (GroupIndex, Int)

p_regex :: P Pattern
p_regex = liftM POr $ sepBy1 p_branch (char '|')

-- man re_format helps a lot, it says one-or-more pieces so this is
-- many1 not many.  Use "()" to indicate an empty piece.
p_branch :: P Pattern
p_branch = liftM PConcat $ many1 p_piece

p_piece :: P Pattern
p_piece = (p_anchor <|> p_atom) >>= p_post_atom -- correct specification

p_atom :: P Pattern
p_atom =  p_group <|> p_bracket <|> p_char <?> "an atom"

group_index :: P (Maybe GroupIndex)
group_index = do
  (gi,ci) <- getState
  let index = succ gi
  setState (index,ci)
  return (Just index)

p_group :: P Pattern
p_group = lookAhead (char '(') >> do
  index <- group_index
  liftM (PGroup index) $ between (char '(') (char ')') p_regex

-- p_post_atom takes the previous atom as a parameter
p_post_atom :: Pattern -> P Pattern
p_post_atom atom = (char '?' >> return (PQuest atom))
               <|> (char '+' >> return (PPlus atom))
               <|> (char '*' >> return (PStar True atom))
               <|> p_bound atom
               <|> return atom

p_bound :: Pattern -> P Pattern
p_bound atom = try $ between (char '{') (char '}') (p_bound_spec atom)

p_bound_spec :: Pattern -> P Pattern
p_bound_spec atom = do lowS <- many1 digit
                       let lowI = read lowS
                       highMI <- option (Just lowI) $ try $ do
                                   _ <- char ','
  -- parsec note: if 'many digits' fails below then the 'try' ensures
  -- that the ',' will not match the closing '}' in p_bound, same goes
  -- for any non '}' garbage after the 'many digits'.
                                   highS <- many digit
                                   if null highS then return Nothing -- no upper bound
                                     else do let highI = read highS
                                             guard (lowI <= highI)
                                             return (Just (read highS))
                       return (PBound lowI highMI atom)

-- An anchor cannot be modified by a repetition specifier
p_anchor :: P Pattern
p_anchor = (char '^' >> liftM PCarat char_index)
       <|> (char '$' >> liftM PDollar char_index)
       <|> try (do _ <- string "()"
                   index <- group_index
                   return $ PGroup index PEmpty)
       <?> "empty () or anchor ^ or $"

char_index :: P DoPa
char_index = do (gi,ci) <- getState
                let ci' = succ ci
                setState (gi,ci')
                return (DoPa ci')

p_char :: P Pattern
p_char = p_dot <|> p_left_brace <|> p_escaped <|> p_other_char where
  p_dot = char '.' >> char_index >>= return . PDot
  p_left_brace = try $ (char '{' >> notFollowedBy digit >> char_index >>= return . (`PChar` '{'))
  p_escaped = char '\\' >> anyChar >>= \c -> char_index >>= return . (`PEscape` c)
  p_other_char = noneOf specials >>= \c -> char_index >>= return . (`PChar` c)
    where specials  = "^.[$()|*+?{\\"

-- parse [bar] and [^bar] sets of characters
p_bracket :: P Pattern
p_bracket = (char '[') >> ( (char '^' >> p_set True) <|> (p_set False) )

p_set :: Bool -> P Pattern
p_set invert = do initial <- option "" (char ']' >> return "]")
                  values <- if null initial then many1 p_set_elem else many p_set_elem
                  _ <- char ']'
                  ci <- char_index
                  let chars = maybe'set $ concat $
                        initial :
                        [ c | BEChar c <- values ] :
                        [ [start..end] | BERange start end <- values ]
                      colls = maybe'set [PatternSetCollatingElement coll | BEColl coll <- values ]
                      equivs = maybe'set [PatternSetEquivalenceClass equiv | BEEquiv equiv <- values]
                      class's = maybe'set [PatternSetCharacterClass a'class | BEClass a'class <- values]
                      maybe'set x = if null x then Nothing else Just (Set.fromList x)
                      sets = PatternSet chars class's colls equivs
                  sets `seq` return $ if invert then PAnyNot ci sets else PAny ci sets

-- From here down the code is the parser and functions for pattern [ ] set things

p_set_elem :: P BracketElement
p_set_elem = checkBracketElement =<< asum
  [ p_set_elem_class
  , p_set_elem_equiv
  , p_set_elem_coll
  , p_set_elem_range
  , p_set_elem_char
  , fail "Failed to parse bracketed string"
  ]

p_set_elem_class :: P BracketElement
p_set_elem_class = liftM BEClass $
  try (between (string "[:") (string ":]") (many1 $ noneOf ":]"))

p_set_elem_equiv :: P BracketElement
p_set_elem_equiv = liftM BEEquiv $
  try (between (string "[=") (string "=]") (many1 $ noneOf "=]"))

p_set_elem_coll :: P BracketElement
p_set_elem_coll =  liftM BEColl $
  try (between (string "[.") (string ".]") (many1 $ noneOf ".]"))

p_set_elem_range :: P BracketElement
p_set_elem_range = try $ do
  start <- noneOf "]"
  _  <- char '-'
  end <- noneOf "]"
  return $ BERange start end

p_set_elem_char :: P BracketElement
p_set_elem_char = do
  c <- noneOf "]"
  return (BEChar c)

-- | Fail when 'BracketElement' is invalid, e.g. empty range @1-0@.
-- This failure should not be caught.
--
checkBracketElement :: BracketElement -> P BracketElement
checkBracketElement e =
  case e of
    BERange start end
      | start > end -> fail $ unwords
          [ "End point"
          , show end
          , "of dashed character range is less than starting point"
          , show start
          ]
      | otherwise -> ok
    BEChar  _ -> ok
    BEClass _ -> ok
    BEColl  _ -> ok
    BEEquiv _ -> ok
  where
    ok = return e
