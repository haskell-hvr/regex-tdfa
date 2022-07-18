{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | This "Text.Regex.TDFA.Pattern" module provides the 'Pattern' data
-- type and its subtypes.  This 'Pattern' type is used to represent
-- the parsed form of a regular expression.

module Text.Regex.TDFA.Pattern
    (Pattern(..)
    ,PatternSet(..)
    ,PatternSetCharacterClass(..)
    ,PatternSetCollatingElement(..)
    ,PatternSetEquivalenceClass(..)
    ,GroupIndex
    ,DoPa(..)
    ,decodeCharacterClass, decodePatternSet
    ,showPattern
-- ** Internal use
    ,starTrans
-- ** Internal use, operations to support debugging under @ghci@
    ,starTrans',simplify',dfsPattern
    ) where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Data.List(intersperse,partition)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Regex.TDFA.Common(DoPa(..),GroupIndex,common_error)

err :: String -> a
err = common_error "Text.Regex.TDFA.Pattern"

-- | 'Pattern' is the type returned by the regular expression parser 'parseRegex'.
-- This is consumed by the "Text.Regex.TDFA.CorePattern" module and the tender leaves
-- are nibbled by the "Text.Regex.TDFA.TNFA" module.
--
-- The 'DoPa' field is the index of the component in the regex string @r@.
data Pattern
  = PEmpty
      -- ^ @()@, matches the empty string.
  | PGroup  (Maybe GroupIndex) Pattern
      -- ^ Group @(r)@.  @Nothing@ indicates non-matching 'PGroup'
      -- (never produced by parser 'parseRegex').
  | POr     [Pattern]
      -- ^ Alternative @r|s@ (flattened by 'starTrans').
  | PConcat [Pattern]
      -- ^ Sequence @rs@ (flattened by 'starTrans').
  | PQuest  Pattern
      -- ^ Zero or one repetitions @r?@ (eliminated by 'starTrans').
  | PPlus   Pattern
      -- ^ One or more repetitions @r+@ (eliminated by 'starTrans').
  | PStar   Bool Pattern
      -- ^ Zero or more repetitions @r*@.
      -- @True@ (default) means may accept the empty string on its first iteration.
  | PBound  Int (Maybe Int) Pattern
      -- ^ Given number or repetitions @r{n}@ or @r{n,m}@
      -- (eliminated by 'starTrans').

  -- The rest of these need an index of where in the regex string it is from
  | PCarat  { getDoPa :: DoPa }
      -- ^ @^@ matches beginning of input.
  | PDollar { getDoPa :: DoPa }
      -- ^ @$@ matches end of input.

  -- The following test and accept a single character
  | PDot    { getDoPa :: DoPa }
      -- ^ @.@ matches any character.
  | PAny    { getDoPa :: DoPa, getPatternSet :: PatternSet }
      -- ^ Bracket expression @[...]@.
  | PAnyNot { getDoPa :: DoPa, getPatternSet :: PatternSet }
      -- ^ Inverted bracket expression @[^...]@.
  | PEscape { getDoPa :: DoPa, getPatternChar :: Char }
      -- ^ Backslashed character @\c@, may have special meaning.
  | PChar   { getDoPa :: DoPa, getPatternChar :: Char }
      -- ^ Single character, matches given character.

  -- The following are semantic tags created in starTrans, not the parser
  | PNonCapture Pattern
     -- ^ Tag for internal use, introduced by 'starTrans'.
  | PNonEmpty Pattern
     -- ^ Tag for internal use, introduced by 'starTrans'.
  deriving (Eq, Show)

-- Andreas Abel, 2022-07-18, issue #47:
-- The following claim is FALSE:
--
-- I have not been checking, but this should have the property that
-- parsing the resulting string should result in an identical 'Pattern'.
-- This is not true if 'starTrans' has created 'PNonCapture' and 'PNonEmpty'
-- values or a @'PStar' False@.  The contents of a @[...]@ grouping are
-- always shown in a sorted canonical order.
showPattern :: Pattern -> String
showPattern pIn =
  case pIn of
    PEmpty -> "()"
    PGroup _ p -> paren (showPattern p)
    POr ps -> concat $ intersperse "|" (map showPattern ps)
    PConcat ps -> concatMap showPattern ps
    PQuest p -> (showPattern p)++"?"
    PPlus p -> (showPattern p)++"+"
    -- If PStar has mayFirstBeNull False then reparsing will forget this flag
    PStar _ p -> (showPattern p)++"*"
    PBound i (Just j) p | i==j -> showPattern p ++ ('{':show i)++"}"
    PBound i mj p -> showPattern p ++ ('{':show i) ++ maybe ",}" (\j -> ',':show j++"}") mj
    --
    PCarat _ -> "^"
    PDollar _ -> "$"
    PDot _ -> "."
    PAny _ ps -> ('[':show ps)++"]"
    PAnyNot _ ps ->  ('[':'^':show ps)++"]"
    PEscape _ c -> '\\':c:[]
    PChar _ c -> [c]
    -- The following were not directly from the parser, and will not be parsed in properly
    PNonCapture p -> showPattern p
    PNonEmpty p -> showPattern p
  where {-
        groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                else (if n <=3 then take n [x..]
                                      else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
        groupRange x n [] = if n <=3 then take n [x..]
                            else x:'-':(toEnum (pred n+fromEnum x)):[]
-}
        paren s = ('(':s)++")"

-- | Content of a bracket expression @[...]@ organized into
-- characters,
-- POSIX character classes (e.g. @[[:alnum:]]@),
-- collating elements (e.g. @[.ch.]@, unused), and
-- equivalence classes (e.g. @[=a=]@, treated as characters).
--
data PatternSet = PatternSet (Maybe (Set Char))
                             (Maybe (Set PatternSetCharacterClass))
                             (Maybe (Set PatternSetCollatingElement))
                             (Maybe (Set PatternSetEquivalenceClass))
                             deriving (Eq)

-- | Hand-rolled implementation, giving textual rather than Haskell representation.
instance Show PatternSet where
  showsPrec i (PatternSet s scc sce sec) =
    let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
        charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
        scc' = maybe "" ((concatMap show) . Set.toList) scc
        sce' = maybe "" ((concatMap show) . Set.toList) sce
        sec' = maybe "" ((concatMap show) . Set.toList) sec
    in shows charSpec
       . showsPrec i scc' . showsPrec i sce' . showsPrec i sec'
       . if '-' `elem` special then showChar '-' else id
    where byRange xAll@(~(x:xs))
            | length xAll <=3 = xAll
            | otherwise       = groupRange x 1 xs
          groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                  else (if n <=3 then take n [x..]
                                        else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
          groupRange x n [] = if n <=3 then take n [x..]
                              else x:'-':(toEnum (pred n+fromEnum x)):[]

-- | Content of @[: :]@, e.g. @"alnum"@ for @[:alnum:]@.
newtype PatternSetCharacterClass   = PatternSetCharacterClass   {unSCC::String}
  deriving (Eq,Ord)

-- | Content of @[. .]@, e.g. @"ch"@ for @[.ch.]@.
newtype PatternSetCollatingElement = PatternSetCollatingElement {unSCE::String}
  deriving (Eq,Ord)

-- | Content of @[= =]@, e.g. @"a"@ for @[=a=]@.
newtype PatternSetEquivalenceClass = PatternSetEquivalenceClass {unSEC::String}
  deriving (Eq,Ord)

-- | Hand-rolled implementation, giving textual rather than Haskell representation.
instance Show PatternSetCharacterClass where
  showsPrec _ p = showChar '[' . showChar ':' . shows (unSCC p) . showChar ':' . showChar ']'

-- | Hand-rolled implementation, giving textual rather than Haskell representation.
instance Show PatternSetCollatingElement where
  showsPrec _ p = showChar '[' . showChar '.' . shows (unSCE p) . showChar '.' . showChar ']'

-- | Hand-rolled implementation, giving textual rather than Haskell representation.
instance Show PatternSetEquivalenceClass where
  showsPrec _ p = showChar '[' . showChar '=' . shows (unSEC p) . showChar '=' . showChar ']'

-- | @decodePatternSet@ cannot handle collating element and treats
-- equivalence classes as just their definition and nothing more.
--
-- @since 1.3.2
decodePatternSet :: PatternSet -> Set Char
decodePatternSet (PatternSet msc mscc _ msec) =
  let baseMSC = maybe Set.empty id msc
      withMSCC = foldl (flip Set.insert) baseMSC  (maybe [] (concatMap decodeCharacterClass . Set.toAscList) mscc)
      withMSEC = foldl (flip Set.insert) withMSCC (maybe [] (concatMap unSEC . Set.toAscList) msec)
  in withMSEC

-- | This returns the strictly ascending list of characters
-- represented by @[: :]@ POSIX character classes.
-- Unrecognized class names return an empty string.
--
-- @since 1.3.2
decodeCharacterClass :: PatternSetCharacterClass -> String
decodeCharacterClass (PatternSetCharacterClass s) =
  case s of
    "alnum"  -> ['0'..'9']++['A'..'Z']++['a'..'z']
    "digit"  -> ['0'..'9']
    "punct"  -> ['\33'..'\47']++['\58'..'\64']++['\91'..'\96']++['\123'..'\126']
    "alpha"  -> ['A'..'Z']++['a'..'z']
    "graph"  -> ['\41'..'\126']
    "space"  -> "\t\n\v\f\r "
    "blank"  -> "\t "
    "lower"  -> ['a'..'z']
    "upper"  -> ['A'..'Z']
    "cntrl"  -> ['\0'..'\31']++"\127" -- with NUL
    "print"  -> ['\32'..'\126']
    "xdigit" -> ['0'..'9']++['A'..'F']++['a'..'f']
    "word"   -> ['0'..'9']++['A'..'Z']++"_"++['a'..'z']
    _ -> []

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==

-- | Do the transformation and simplification in a single traversal.
-- This removes the 'PPlus', 'PQuest', and 'PBound' values, changing to 'POr'
-- and 'PEmpty' and 'PStar'.  For some 'PBound' values it adds
-- 'PNonEmpty' and 'PNonCapture' semantic marker.  It also simplifies to
-- flatten out nested 'POr' and 'PConcat' instances and eliminate some
-- unneeded 'PEmpty' values.
starTrans :: Pattern -> Pattern
starTrans = dfsPattern (simplify' . starTrans')

-- | Apply a 'Pattern' transformation function depth first.
dfsPattern :: (Pattern -> Pattern)  -- ^ The transformation function.
           -> Pattern               -- ^ The 'Pattern' to transform.
           -> Pattern               -- ^ The transformed 'Pattern'.
dfsPattern f = dfs
 where unary c = f . c . dfs
       dfs pattern = case pattern of
                       POr ps -> f (POr (map dfs ps))
                       PConcat ps -> f (PConcat (map dfs ps))
                       PGroup i p -> unary (PGroup i) p
                       PQuest p -> unary PQuest p
                       PPlus p -> unary PPlus p
                       PStar i p -> unary (PStar i) p
                       PBound i mi p -> unary (PBound i mi) p
                       _ -> f pattern

{- Replace by PNonCapture
unCapture = dfsPattern unCapture' where
  unCapture' (PGroup (Just _) p) = PGroup Nothing p
  unCapture' x = x
-}
reGroup :: Pattern -> Pattern
reGroup p@(PConcat xs) | 2 <= length xs = PGroup Nothing p
reGroup p@(POr xs)     | 2 <= length xs = PGroup Nothing p
reGroup p = p

starTrans' :: Pattern -> Pattern
starTrans' pIn =
  case pIn of -- We know that "p" has been simplified in each of these cases:
    PQuest p -> POr [p,PEmpty]

{- The PStar should not capture 0 characters on its first iteration,
   so set its mayFirstBeNull flag to False
 -}
    PPlus p | canOnlyMatchNull p -> p
            | otherwise -> asGroup $ PConcat [reGroup p,PStar False p]

{- "An ERE matching a single character repeated by an '*' , '?' , or
   an interval expression shall not match a null expression unless
   this is the only match for the repetition or it is necessary to
   satisfy the exact or minimum number of occurrences for the interval
   expression."
 -}
{- p? is p|PEmpty which prefers even a 0-character match for p
   p{0,1} is p? is POr [p,PEmpty]
   p{0,2} is (pp?)? NOT p?p?
   p{0,3} is (p(pp?)?)?
   p{1,2} is like pp{0,1} is like pp? but see below
   p{2,5} is ppp{0,3} is pp(p(pp?)?)?

   But this is not always right.  Because if the second use of p in
   p?p? matches 0 characters then the perhaps non 0 character match of
   the first p is overwritten.

   We need a new operation "p!" that means "p?" unless "p" match 0
   characters, in which case skip p as if it failed in "p?".  Thus
   when p cannot accept 0 characters p! and p? are equivalent.  And
   when p can only match 0 characters p! is PEmpty.  So for
   simplicity, only use ! when p can match 0 characters but not only 0
   characters.

   Call this (PNonEmpty p) in the Pattern type.
   p! is PNonEmpty p is POr [PEmpty,p]
   IS THIS TRUE?  Use QuickCheck?

   Note that if p cannot match 0 characters then p! is p? and vice versa

   The p{0,1} is still always p? and POr [p,PEmpty]
   Now p{0,2} means p?p! or (pp!)? and p{0,3} means (p(pp!)!)? or p?p!p!
   Equivalently p?p! and p?p!p!
   And p{2,2} is p'p and p{3,3} is p'p'p and p{4} is p'p'p'p
   The p{1,2} is pp! and p{1,3} is pp!p! or p(pp!)!
   And p{2,4} means p'pp!p! and p{3,6} is p'p'pp!p!p! or p'p'p(p(pp!)!)!

   But this second form still has a problem: the (pp!)! can have the first
   p match 0 and the second p match non-zero. This showed up for (.|$){1,3}
   since ($.!)! should not be a valid path but altered the qt_win commands.

   Thus only p'p'pp!p!p! has the right semantics.  For completeness:

   if p can only match only 0 characters then the cases are
   p{0,0} is (), p{0,_} = p?, p{_,_} is p

   if p can match 0 or non-zero characters then cases are
   p{0,0} is (), p{0,1} is (p)?, p{0,2} is (pp!)?, p{0,3} is (pp!p!)?
   p{1,1} is p, p{1,2} is pp!, p{1,3} is pp!p!, p{1,4} is pp!p!p!
   p{2,2} is p'p,
   p{2,3} is p'pp!,
   p{2,4} is p'pp!p! or p'p(pp!)!
   p{2,5} is p'pp!p!p! or p'p(p(pp!)!)!
   p{3,3} is p'p'p, p{3,4} is p'p'pp!, p{3,5} is p'p'pp!p!, p{3,6} is p'p'pp!p!p!

   if p can only match 1 or more characters then cases are
   p{0,0} is ()
   p{0,1} is p?, p{0,2} is (pp?)?, p{0,3} is (p(pp?)?)?, p{0,4} is (pp{0,3})?
   p{1,1} is p, p{1,j} is pp{0,pred j}
   p{2,2} is p'p, p{2,3} is p'pp?, p{2,4} is p'p(pp?)?, p{2,5} = p'p{1,4} = p'(pp{0,3})
   p{3,3} is p'p'p, p{3,4} is p'p'pp?, p{3,5} is p'p'p(pp?)?, p{3,6} is

   And by this logic, the PStar False is really p*!  So p{0,} is p*
   and p{1,} is pp*! and p{2,} is p'pp*! and p{3,} is p'p'pp*!

   The (nonEmpty' p) below is the only way PNonEmpty is introduced
   into the Pattern.  It is always preceded by p inside a PConcat
   list.  The p involved never simplifies to PEmpty.  Thus it is
   impossible to have PNonEmpty directly nested, i.e. (PNonEmpty
   (PNonEmpty _)) never occurs even after simplifications.

   The (nonCapture' p) below is the only way PNonCapture is
   introduced into the Pattern. It is always followed by p inside a
   PConcat list.

-}
-- Easy cases
    PBound i _        _ | i<0 -> PEmpty  -- impossibly malformed
    PBound i (Just j) _ | i>j -> PEmpty  -- impossibly malformed
    PBound _ (Just 0) _ -> PEmpty
-- Medium cases
    PBound 0 Nothing  p | canOnlyMatchNull p -> quest p
                        | otherwise -> PStar True p
    PBound 0 (Just 1) p -> quest p
-- Hard cases
    PBound i Nothing  p | canOnlyMatchNull p -> p
                        | otherwise -> asGroup . PConcat $ apply (nc'p:) (pred i) [reGroup p,PStar False p]
      where nc'p = nonCapture' p
    PBound 0 (Just j) p | canOnlyMatchNull p -> quest p
                        -- The first operation is quest NOT nonEmpty. This can be tested with
                        -- "a\nb" "((^)?|b){0,3}" and "a\nb" "((^)|b){0,3}"
                        | otherwise -> quest . (concat' p) $
                                        apply (nonEmpty' . (concat' p)) (j-2) (nonEmpty' p)
{- 0.99.6 remove
| cannotMatchNull p -> apply (quest' . (concat' p)) (pred j) (quest' p)
| otherwise -> POr [ simplify' (PConcat (p : replicate (pred j) (nonEmpty' p))) , PEmpty ]
-}
{- 0.99.6 add, 0.99.7 remove
    PBound i (Just j) p | canOnlyMatchNull p -> p
                        | i == j -> PConcat $ apply (p':) (pred i) [p]
                        | otherwise -> PConcat $ apply (p':) (pred i)
                                        [p,apply (nonEmpty' . (concat' p)) (j-i-1) (nonEmpty' p) ]
      where p' = nonCapture' p
-}
{- 0.99.7 add -}
    PBound i (Just j) p | canOnlyMatchNull p -> p
                        | i == j -> asGroup . PConcat $ apply (nc'p:) (pred i) [reGroup p]
                        | otherwise -> asGroup . PConcat $ apply (nc'p:) (pred i)
                                        [reGroup p,apply (nonEmpty' . (concat' p)) (j-i-1) (ne'p) ]
      where nc'p = nonCapture' p
            ne'p = nonEmpty' p
{- 0.99.6
| cannotMatchNull p -> PConcat $ apply (p':) (pred i) $ (p:) $
  [apply (quest' . (concat' p)) (pred (j-i)) (quest' p)]
| otherwise -> PConcat $ (replicate (pred i) p') ++ p : (replicate (j-i) (nonEmpty' p))
-}
    PStar mayFirstBeNull p | canOnlyMatchNull p -> if mayFirstBeNull then quest p
                                                                    else PEmpty
                           | otherwise -> pass
    -- Left intact
    PEmpty -> pass
    PGroup {} -> pass
    POr {} -> pass
    PConcat {} -> pass
    PCarat {} -> pass
    PDollar {} -> pass
    PDot {} -> pass
    PAny {} -> pass
    PAnyNot {} -> pass
    PEscape {} -> pass
    PChar {} -> pass
    PNonCapture {} -> pass
    PNonEmpty {} -> pass -- TODO : remove PNonEmpty from program
  where
    quest = (\ p -> POr [p,PEmpty])  -- require p to have been simplified
--    quest' = (\ p -> simplify' $ POr [p,PEmpty])  -- require p to have been simplified
    concat' a b = simplify' $ PConcat [reGroup a,reGroup b]      -- require a and b to have been simplified
    nonEmpty' = (\ p -> simplify' $ POr [PEmpty,p]) -- 2009-01-19 : this was PNonEmpty
    nonCapture' = PNonCapture
    apply f n x = foldr ($) x (replicate n f) -- function f applied n times to x : f^n(x)
    asGroup p = PGroup Nothing (simplify' p)
    pass = pIn

-- | Function to transform a pattern into an equivalent, but less
-- redundant form.  Nested 'POr' and 'PConcat' are flattened. 'PEmpty'
-- is propagated.
simplify' :: Pattern -> Pattern
simplify' x@(POr _) =
  let ps' = case span notPEmpty (flatten x) of
              (notEmpty,[]) -> notEmpty
              (notEmpty,_:rest) -> notEmpty ++ (PEmpty:filter notPEmpty rest) -- keep 1st PEmpty only
  in case ps' of
       [] -> PEmpty
       [p] -> p
       _ -> POr ps'
simplify' x@(PConcat _) =
  let ps' = filter notPEmpty (flatten x)
  in case ps' of
       [] -> PEmpty
       [p] -> p
       _ -> PConcat ps' -- PConcat ps'
simplify' (PStar _ PEmpty) = PEmpty
simplify' (PNonCapture PEmpty) = PEmpty -- 2009, perhaps useful
--simplify' (PNonEmpty PEmpty) = err "simplify' (PNonEmpty PEmpty) = should be Impossible!" -- 2009
simplify' other = other

-- | Function to flatten nested 'POr' or nested 'PConcat' applicataions.
flatten :: Pattern -> [Pattern]
flatten (POr ps) = (concatMap (\x -> case x of
                                       POr ps' -> ps'
                                       p -> [p]) ps)
flatten (PConcat ps) = (concatMap (\x -> case x of
                                           PConcat ps' -> ps'
                                           p -> [p]) ps)
flatten _ = err "flatten can only be applied to POr or PConcat"

notPEmpty :: Pattern -> Bool
notPEmpty PEmpty = False
notPEmpty _      = True

-- | Determines if 'Pattern' will fail or accept @[]@ and never accept any
-- characters. Treat 'PCarat' and 'PDollar' as @True@.
canOnlyMatchNull :: Pattern -> Bool
canOnlyMatchNull pIn =
  case pIn of
    PEmpty -> True
    PGroup _ p -> canOnlyMatchNull p
    POr ps -> all canOnlyMatchNull ps
    PConcat ps -> all canOnlyMatchNull ps
    PQuest p -> canOnlyMatchNull p
    PPlus p -> canOnlyMatchNull p
    PStar _ p -> canOnlyMatchNull p
    PBound _ (Just 0) _ -> True
    PBound _ _ p -> canOnlyMatchNull p
    PCarat _ -> True
    PDollar _ -> True
    PNonCapture p -> canOnlyMatchNull p
--    PNonEmpty p -> canOnlyMatchNull p -- like PQuest
    _ ->False

{-

-- | If 'cannotMatchNull' returns 'True' then it is known that the
-- 'Pattern' will never accept an empty string.  If 'cannotMatchNull'
-- returns 'False' then it is possible but not definite that the
-- 'Pattern' could accept an empty string.
cannotMatchNull :: Pattern -> Bool
cannotMatchNull pIn =
  case pIn of
    PEmpty -> False
    PGroup _ p -> cannotMatchNull p
    POr [] -> False
    POr ps -> all cannotMatchNull ps
    PConcat [] -> False
    PConcat ps -> any cannotMatchNull ps
    PQuest _ -> False
    PPlus p -> cannotMatchNull p
    PStar {} -> False
    PBound 0 _ _ -> False
    PBound _ _ p -> cannotMatchNull p
    PCarat _ -> False
    PDollar _ -> False
    PNonCapture p -> cannotMatchNull p
--    PNonEmpty _ -> False -- like PQuest
    _ -> True
-}
