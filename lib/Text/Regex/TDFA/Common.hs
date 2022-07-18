{-# OPTIONS -funbox-strict-fields #-}

-- | Common provides simple functions to the backend.
-- It defines most of the data types.
-- All modules should call 'error' via the 'common_error' function below.

module Text.Regex.TDFA.Common where

import Text.Regex.Base(RegexOptions(..))

{- By Chris Kuklewicz, 2007-2009. BSD License, see the LICENSE file. -}
import Data.Array.IArray(Array)
import Data.IntSet.EnumSet2(EnumSet)
import qualified Data.IntSet.EnumSet2 as Set(toList)
import Data.IntMap.CharMap2(CharMap(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap (findWithDefault,assocs,toList,null,size,toAscList)
import Data.IntSet(IntSet)
import qualified Data.IntMap.CharMap2 as Map (assocs,toAscList,null)
import Data.Sequence as S(Seq)
--import Debug.Trace

import Text.Regex.TDFA.IntArrTrieSet(TrieSet)

{-# INLINE look #-}
look :: Int -> IntMap a -> a
look key imap = IMap.findWithDefault (common_error "Text.Regex.DFA.Common" ("key "++show key++" not found in look")) key imap

common_error :: String -> String -> a
common_error moduleName message =
  error ("Explict error in module "++moduleName++" : "++message)

on :: (t1 -> t1 -> t2) -> (t -> t1) -> t -> t -> t2
f `on` g = (\x y -> (g x) `f` (g y))

-- | After 'sort' or 'sortBy' the use of 'nub' or 'nubBy' can be replaced by 'norep' or 'norepBy'.
norep :: (Eq a) => [a]->[a]
norep [] = []
norep x@[_] = x
norep (a:bs@(c:cs)) | a==c = norep (a:cs)
                    | otherwise = a:norep bs

-- | After 'sort' or 'sortBy' the use of 'nub' or 'nubBy' can be replaced by 'norep' or 'norepBy'.
norepBy :: (a -> a -> Bool) -> [a] -> [a]
norepBy _ [] = []
norepBy _ x@[_] = x
norepBy eqF (a:bs@(c:cs)) | a `eqF` c = norepBy eqF (a:cs)
                          | otherwise = a:norepBy eqF bs

mapFst :: (Functor f) => (t -> t2) -> f (t, t1) -> f (t2, t1)
mapFst f = fmap (\ (a,b) -> (f a,b))

mapSnd :: (Functor f) => (t1 -> t2) -> f (t, t1) -> f (t, t2)
mapSnd f = fmap (\ (a,b) -> (a,f b))

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

noWin :: WinTags -> Bool
noWin = null

-- | Used to track elements of the pattern that accept characters or are anchors.
newtype DoPa = DoPa {dopaIndex :: Int} deriving (Eq,Ord)

instance Enum DoPa where
  toEnum = DoPa
  fromEnum = dopaIndex

instance Show DoPa where
  showsPrec p (DoPa {dopaIndex=i}) = ('#':) . showsPrec p i

-- | Control whether the pattern is multiline or case-sensitive like Text.Regex and whether to
-- capture the subgroups (\\1, \\2, etc).  Controls enabling extra anchor syntax.
data CompOption = CompOption {
    caseSensitive :: Bool
      -- ^ True in 'blankCompOpt' and 'defaultCompOpt'.
  , multiline :: Bool
      -- ^ False in 'blankCompOpt', True in 'defaultCompOpt'.
      -- Compile for newline-sensitive matching.
      --
      -- From [regexp man page](https://www.tcl.tk/man/tcl8.4/TclCmd/regexp.html#M8):
      -- "By default, newline is a completely ordinary character with no special meaning in either REs or strings.
      -- With this flag, inverted bracket expressions @[^@ and @.@ never match newline,
      -- a @^@ anchor matches the null string after any newline in the string in addition to its normal function,
      -- and the @$@ anchor matches the null string before any newline in the string in addition to its normal function."
  , rightAssoc :: Bool
      -- ^ True (and therefore right associative) in 'blankCompOpt' and 'defaultCompOpt'.
  , newSyntax :: Bool
      -- ^ False in 'blankCompOpt', True in 'defaultCompOpt'.
      -- Enables the extended non-POSIX syntax described in "Text.Regex.TDFA" haddock documentation.
  , lastStarGreedy ::  Bool
      -- ^ False by default.  This is POSIX correct but it takes space and is slower.
      -- Setting this to True will improve performance, and should be done
      -- if you plan to set the 'captureGroups' 'ExecOption' to False.
  } deriving (Read,Show)

data ExecOption = ExecOption {
    captureGroups :: Bool    -- ^ True by default.  Set to False to improve speed (and space).
  } deriving (Read,Show)

-- | Used by implementation to name certain 'Postion's during
-- matching. Identity of 'Position' tag to set during a transition.
type Tag = Int

-- | Internal use to indicate type of tag and preference for larger or smaller 'Position's.
data OP = Maximize | Minimize | Orbit | Ignore deriving (Eq,Show)

-- | Internal NFA node identity number.
type Index = Int

-- | Internal DFA identity is this 'Set' of NFA 'Index'.
type SetIndex = IntSet {- Index -}

-- | Index into the text being searched.
type Position = Int

-- | 'GroupIndex' is for indexing submatches from capturing parenthesized groups ('PGroup' or 'Group').
type GroupIndex = Int

-- | 'GroupInfo' collects the parent and tag information for an instance of a group.
data GroupInfo = GroupInfo {
    thisIndex, parentIndex :: GroupIndex
  , startTag, stopTag, flagTag :: Tag
  } deriving Show

-- | The TDFA backend specific 'Regex' type, used by this module's 'RegexOptions' and 'RegexMaker'.
data Regex = Regex {
    regex_dfa :: DFA                             -- ^ starting DFA state
  , regex_init :: Index                          -- ^ index of starting state
  , regex_b_index :: (Index,Index)               -- ^ indexes of smallest and largest states
  , regex_b_tags :: (Tag,Tag)                    -- ^ indexes of smallest and largest tags
  , regex_trie :: TrieSet DFA                    -- ^ All DFA states
  , regex_tags :: Array Tag OP                   -- ^ information about each tag
  , regex_groups :: Array GroupIndex [GroupInfo] -- ^ information about each group
  , regex_isFrontAnchored :: Bool                -- ^ used for optimizing execution
  , regex_compOptions :: CompOption
  , regex_execOptions :: ExecOption
  } -- no deriving at all, the DFA may be too big to ever traverse!


instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt =  CompOption { caseSensitive = True
                             , multiline = False
                             , rightAssoc = True
                             , newSyntax = False
                             , lastStarGreedy = False
                             }
  blankExecOpt =  ExecOption { captureGroups = True }
  defaultCompOpt = CompOption { caseSensitive = True
                              , multiline = True
                              , rightAssoc = True
                              , newSyntax = True
                              , lastStarGreedy = False
                              }
  defaultExecOpt =  ExecOption { captureGroups = True }
  setExecOpts e r = r {regex_execOptions=e}
  getExecOpts r = regex_execOptions r


data WinEmpty = WinEmpty Instructions
              | WinTest WhichTest (Maybe WinEmpty) (Maybe WinEmpty)
  deriving Show

-- | Internal NFA node type.
data QNFA = QNFA {q_id :: Index, q_qt :: QT}

-- | Internal to 'QNFA' type.
data QT = Simple { qt_win :: WinTags -- ^ empty transitions to the virtual winning state
                 , qt_trans :: CharMap QTrans -- ^ all ways to leave this QNFA to other or the same QNFA
                 , qt_other :: QTrans -- ^ default ways to leave this QNFA to other or the same QNFA
                 }
        | Testing { qt_test :: WhichTest -- ^ The test to perform
                  , qt_dopas :: EnumSet DoPa  -- ^ location(s) of the anchor(s) in the original regexp
                  , qt_a, qt_b :: QT -- ^ use qt_a if test is True, else use qt_b
                  }

-- | Internal type to represent the tagged transition from one QNFA to
-- another (or itself).  The key is the Index of the destination QNFA.
type QTrans = IntMap {- Destination Index -} [TagCommand]

-- | Known predicates, just Beginning of Line (^) and End of Line ($).
-- Also support for GNU extensions is being added: \\\` beginning of
-- buffer, \\\' end of buffer, \\\< and \\\> for begin and end of words, \\b
-- and \\B for word boundary and not word boundary.
data WhichTest
  = Test_BOL          -- ^ @^@ (affected by multiline option)
  | Test_EOL          -- ^ @$@ (affected by multiline option)
  | Test_BOB          -- ^ @\\`@ beginning of buffer
  | Test_EOB          -- ^ @\\'@ end ofbuffer
  | Test_BOW          -- ^ @\\<@ beginning of word
  | Test_EOW          -- ^ @\\>@ end of word
  | Test_EdgeWord     -- ^ @\\b@ word boundary
  | Test_NotEdgeWord  -- ^ @\\B@ not word boundary
  deriving (Show,Eq,Ord,Enum)

-- | The things that can be done with a Tag.  'TagTask' and
-- 'ResetGroupStopTask' are for tags with Maximize or Minimize OP
-- values.  'ResetOrbitTask' and 'EnterOrbitTask' and 'LeaveOrbitTask' are
-- for tags with Orbit OP value.
data TagTask = TagTask | ResetGroupStopTask | SetGroupStopTask
             | ResetOrbitTask | EnterOrbitTask | LeaveOrbitTask deriving (Show,Eq)

-- | Ordered list of tags and their associated Task.
type TagTasks = [(Tag,TagTask)]

-- | When attached to a QTrans the TagTask can be done before or after
-- accepting the character.
data TagUpdate = PreUpdate TagTask | PostUpdate TagTask deriving (Show,Eq)

-- | Ordered list of tags and their associated update operation.
type TagList = [(Tag,TagUpdate)]

-- | A TagList and the location of the item in the original pattern
-- that is being accepted.
type TagCommand = (DoPa,TagList)

-- | Ordered list of tags and their associated update operation to
-- perform on an empty transition to the virtual winning state.
type WinTags = TagList

-- | Internal DFA node, identified by the Set of indices of the QNFA
-- nodes it represents.
data DFA = DFA { d_id :: SetIndex, d_dt :: DT } deriving(Show)
data Transition = Transition { trans_many :: DFA    -- ^ where to go (maximal), including respawning
                             , trans_single :: DFA  -- ^ where to go, not including respawning
                             , trans_how :: DTrans    -- ^ how to go, including respawning
                             }
-- | Internal to the DFA node
data DT = Simple' { dt_win :: IntMap {- Source Index -} Instructions -- ^ Actions to perform to win
                  , dt_trans :: CharMap Transition -- ^ Transition to accept Char
                  , dt_other :: Transition -- ^ default accepting transition
                  }
        | Testing' { dt_test :: WhichTest -- ^ The test to perform
                   , dt_dopas :: EnumSet DoPa -- ^ location(s) of the anchor(s) in the original regexp
                   , dt_a,dt_b :: DT      -- ^ use dt_a if test is True else use dt_b
                   }

-- | Internal type to represent the commands for the tagged transition.
-- The outer 'IntMap' is for the destination Index and the inner 'IntMap'
-- is for the Source Index.  This is convenient since all runtime data
-- going to the same destination must be compared to find the best.
--
-- A Destination 'IntMap' entry may have an empty Source 'IntMap' if and
-- only if the destination is the starting index and the NFA or DFA.
-- This instructs the matching engine to spawn a new entry starting at
-- the post-update position.
type DTrans = IntMap {- Index of Destination -} (IntMap {- Index of Source -} (DoPa,Instructions))
-- type DTrans = IntMap {- Index of Destination -} (IntMap {- Index of Source -} (DoPa,RunState ()))

-- | Internal convenience type for the text display code.
type DTrans' = [(Index, [(Index, (DoPa, ([(Tag, (Position,Bool))],[String])))])]

-- | Positions for which a @*@ was re-started while looping.  Need to
-- append locations at back but compare starting with front, so use
-- 'Seq' as a queue.  The initial position is saved in 'basePos' (and a
-- Maximize Tag), the middle positions in the 'Seq', and the final
-- position is NOT saved in the Orbits (only in a Maximize Tag).
data Orbits = Orbits
  { inOrbit :: !Bool        -- True if enterOrbit, False if LeaveOrbit
  , basePos :: Position
  , ordinal :: (Maybe Int)
  , getOrbits :: !(Seq Position)
  } deriving (Show)

-- | The 'newPos' and 'newFlags' lists in Instructions are sorted by, and unique in, the Tag values
data Instructions = Instructions
  { newPos :: ![(Tag,Action)] -- False is preUpdate, True is postUpdate (there are no Orbit tags here) -- 2009 : Change to enum from bool?
  , newOrbits :: !(Maybe (Position -> OrbitTransformer))
  }

instance Show Instructions where
  showsPrec p (Instructions pos _)
    = showParen (p >= 11) $
        showString "Instructions {" .
        showString "newPos = " .
        showsPrec 0 pos .
        showString ", " .
        showString "newOrbits = " .
        showString "<function>" .
        showString "}"

data Action = SetPre | SetPost | SetVal Int deriving (Show,Eq)
type OrbitTransformer = OrbitLog -> OrbitLog
type OrbitLog = IntMap Orbits

instance Show QNFA where
  show (QNFA {q_id = i, q_qt = qt}) = "QNFA {q_id = "++show i
                                  ++"\n     ,q_qt = "++ show qt
                                  ++"\n}"

instance Show QT where
  show = showQT

showQT :: QT -> String
showQT (Simple win trans other) = "{qt_win=" ++ show win
                             ++ "\n, qt_trans=" ++ show (foo trans)
                             ++ "\n, qt_other=" ++ show (foo' other) ++ "}"
  where foo :: CharMap QTrans -> [(Char,[(Index,[TagCommand])])]
        foo = mapSnd foo' . Map.toAscList
        foo' :: QTrans -> [(Index,[TagCommand])]
        foo' = IMap.toList
showQT (Testing test dopas a b) = "{Testing "++show test++" "++show (Set.toList dopas)
                              ++"\n"++indent' a
                              ++"\n"++indent' b++"}"
    where indent' = init . unlines . map (spaces++) . lines . showQT
          spaces = replicate 9 ' '

instance Show DT where show = showDT

indent :: [String] -> String
indent = unlines . map (\x -> ' ':' ':x)

showDT :: DT -> String
showDT (Simple' w t o) =
       "Simple' { dt_win = " ++ seeWin1
  ++ "\n        , dt_trans = " ++ seeTrans1
  ++ "\n        , dt_other = " ++ seeOther1 o
  ++ "\n        }"
 where
  seeWin1 | IMap.null w = "No win"
          | otherwise = indent . map show . IMap.assocs $ w

  seeTrans1 :: String
  seeTrans1 | Map.null t = "No (Char,Transition)"
            | otherwise = ('\n':) . indent $
     map (\(char,Transition {trans_many=dfa,trans_single=dfa2,trans_how=dtrans}) ->
                           concat ["("
                                  ,show char
                                  ,", MANY "
                                  ,show (d_id dfa)
                                  ,", SINGLE "
                                  ,show (d_id dfa2)
                                  ,", \n"
                                  ,seeDTrans dtrans
                                  ,")"]) (Map.assocs t)

  seeOther1 (Transition {trans_many=dfa,trans_single=dfa2,trans_how=dtrans}) =
    concat ["(MANY "
           ,show (d_id dfa)
           ,", SINGLE "
           ,show (d_id dfa2)
           ,", \n"
           ,seeDTrans dtrans
           ,")"]

showDT (Testing' wt d a b) = "Testing' { dt_test = " ++ show wt
                          ++ "\n         , dt_dopas = " ++ show d
                          ++ "\n         , dt_a = " ++ indent' a
                          ++ "\n         , dt_b = " ++ indent' b
                          ++ "\n         }"
 where indent' = init . unlines . (\s -> case s of
                                           [] -> []
                                           (h:t) -> h : (map (spaces ++) t)) . lines . showDT
       spaces = replicate 10 ' '


seeDTrans :: DTrans -> String
--seeDTrans x = concatMap (\(dest,y) -> unlines . map (\(source,ins) -> show (dest,source,ins) ) . IMap.assocs $ y) (IMap.assocs x)
seeDTrans x | IMap.null x = "No DTrans"
seeDTrans x = concatMap seeSource (IMap.assocs x)
  where seeSource (dest,srcMap) | IMap.null srcMap = indent [show (dest,"SPAWN")]
                                | otherwise = indent . map (\(source,ins) -> show (dest,source,ins) ) . IMap.assocs $ srcMap
--        spawnIns = Instructions { newPos = [(0,SetPost)], newOrbits = Nothing }


instance Eq QT where
  t1@(Testing {}) == t2@(Testing {}) =
    (qt_test t1) == (qt_test t2) && (qt_a t1) == (qt_a t2) && (qt_b t1) == (qt_b t2)
  (Simple w1 (CharMap t1) o1) == (Simple w2 (CharMap t2) o2) =
    w1 == w2 && eqTrans && eqQTrans o1 o2
    where eqTrans :: Bool
          eqTrans = (IMap.size t1 == IMap.size t2)
                    && and (zipWith together (IMap.toAscList t1) (IMap.toAscList t2))
            where together (c1,qtrans1) (c2,qtrans2) = (c1 == c2) && eqQTrans qtrans1 qtrans2
          eqQTrans :: QTrans -> QTrans -> Bool
          eqQTrans = (==)
  _ == _ = False
