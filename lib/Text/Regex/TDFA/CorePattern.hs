-- | The CorePattern module deconstructs the Pattern tree created by
-- ReadRegex.parseRegex and returns a simpler Q\/P tree with
-- annotations at each Q node.  This will be converted by the TNFA
-- module into a QNFA finite automata.
--
-- Of particular note, this Pattern to Q\/P conversion creates and
-- assigns all the internal Tags that will be used during the matching
-- process, and associates the captures groups with the tags that
-- represent their starting and ending locations and with their
-- immediate parent group.
--
-- Each Maximize and Minimize tag is held as either a preTag or a
-- postTag by one and only one location in the Q\/P tree.  The Orbit
-- tags are each held by one and only one Star node.  Tags that stop a
-- Group are also held in perhaps numerous preReset lists.
--
-- The additional nullQ::nullView field of Q records the potentially
-- complex information about what tests and tags must be used if the
-- pattern unQ::P matches 0 zero characters.  There can be redundancy
-- in nullView, which is eliminated by cleanNullView.
--
-- Uses recursive do notation.
--
-- 2009 XXX TODO: we can avoid needing tags in the part of the pattern
-- after the last capturing group (when right-associative).  This is
-- flipped for left-associative where the front of the pattern before
-- the first capturing group needs no tags.  The edge of these regions
-- is subtle: both case needs a Maximize tag.  One ought to be able to
-- check the Pattern: if the root is PConcat then a scan from the end
-- (start) looking for the first with an embedded PGroup can be found
-- and the PGroup free elements can be wrapped in some new PNOTAG
-- semantic indicator.
module Text.Regex.TDFA.CorePattern(Q(..),P(..),WhichTest(..),Wanted(..)
                                  ,TestInfo,OP(..),SetTestInfo(..),NullView
                                  ,patternToQ,cleanNullView,cannotAccept,mustAccept) where

import Control.Monad (liftM2, forM, replicateM)
import Control.Monad.RWS (RWS, runRWS, ask, local, listens, tell, get, put)

import Data.Array.IArray(Array,(!),accumArray,listArray)
import Data.Either (partitionEithers, rights)
import Data.List(sort)
import Data.IntMap.EnumMap2(EnumMap)
import qualified Data.IntMap.EnumMap2 as Map(singleton,null,assocs,keysSet)
--import Data.Maybe(isNothing)
import Data.IntSet.EnumSet2(EnumSet)
import qualified Data.IntSet.EnumSet2 as Set(singleton,toList,isSubsetOf)
import Data.Semigroup as Sem

import Text.Regex.TDFA.Common {- all -}
import Text.Regex.TDFA.Pattern(Pattern(..),starTrans)
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}


--err :: String -> a
--err = common_error "Text.Regex.TDFA.CorePattern"

--debug :: (Show a) => a -> b -> b
--debug _ = id

-- Core Pattern Language
data P = Empty                       -- Could be replaced by (Test Nothing)??
       | Or [Q]
       | Seq Q Q
       | Star { getOrbit :: Maybe Tag -- tag to prioritize the need to keep track of length of each pass though q
              , resetOrbits :: [Tag]  -- child star's orbits to reset (ResetOrbitTask) at all depths
              , firstNull :: Bool     -- Usually True to mean the first pass may match 0 characters
              , unStar :: Q}
       | Test TestInfo               -- Require the test to be true (merge with empty as (Test (Maybe TestInfo)) ??)
       | OneChar Pattern             -- Bring the Pattern element that accepts a character
       | NonEmpty Q                  -- Don't let the Q pattern match nothing
         deriving (Show,Eq)

-- The diagnostics about the pattern.  Note that when unQ is 'Seq' the
-- the preTag and postTag are Nothing but the preReset might have tags
-- from PGroup injecting them.
data Q = Q {nullQ :: NullView                  -- Ordered list of nullable views
           ,takes :: (Position,Maybe Position) -- Range of number of accepted characters
           ,preReset :: [Tag]                  -- Tags to "reset" (ResetGroupStopTask) (Only immediate children for efficiency)
           ,postSet :: [Tag]                   -- Tags to "set" (SetGroupStopTask)
           ,preTag,postTag :: Maybe Tag        -- Tags assigned around this pattern (TagTask)
           ,tagged :: Bool                     -- Whether this node should be tagged -- patternToQ use only
           ,childGroups :: Bool                -- Whether unQ has any PGroups -- patternToQ use only
           ,wants :: Wanted                    -- What kind of continuation is used by this pattern
           ,unQ :: P} deriving (Eq)

type TestInfo = (WhichTest,DoPa)

-- This is newtype'd to allow control over class instances
-- This is a set of WhichTest where each test has associated pattern location information
newtype SetTestInfo = SetTestInfo {getTests :: EnumMap WhichTest (EnumSet DoPa)} deriving (Eq)

instance Semigroup SetTestInfo where
  SetTestInfo x <> SetTestInfo y = SetTestInfo (x Sem.<> y)

instance Monoid SetTestInfo where
  mempty = SetTestInfo mempty
  mappend = (Sem.<>)

instance Show SetTestInfo where
  show (SetTestInfo sti) = "SetTestInfo "++show (mapSnd (Set.toList) $ Map.assocs sti)

-- There may be several distinct ways for a subtree to conditionally
-- (i.e. with a Test) or unconditionally accept 0 characters.  These
-- are in the list in order of preference, with most preferred listed
-- first.
type NullView = [(SetTestInfo,TagList)]  -- Ordered list of null views, each is a set of tests and tags

-- During the depth first traversal, children are told about tags by the parent.
-- They may change Apply to Advice and they may generate new tags.
data HandleTag = NoTag             -- No tag at this boundary
               | Advice Tag        -- tag at this boundary, applied at higher level in tree
               | Apply Tag         -- tag at this boundary, may be applied at this node or passed to one child
                 deriving (Show)

-- Nodes in the tree are labeled by the type kind of continuation they
-- prefer to be passed when processing.  This makes it possible to
-- create a smaller number of QNFA states and avoid creating wasteful
-- QNFA states that won't be reachable in the final automata.
--
-- In practice WantsBoth is treated identically to WantsQNFA and
-- WantsBoth could be removed.
data Wanted = WantsQNFA | WantsQT | WantsBoth | WantsEither deriving (Eq,Show)

instance Show Q where
  show = showQ

showQ :: Q -> String
showQ q = "Q { nullQ = "++show (nullQ q)++
        "\n  , takes = "++show (takes q)++
        "\n  , preReset = "++show (preReset q)++
        "\n  , postSet = "++show (postSet q)++
        "\n  , preTag = "++show (preTag q)++
        "\n  , postTag = "++show (postTag q)++
        "\n  , tagged = "++show (tagged q)++
        "\n  , wants = "++show (wants q)++
        "\n  , unQ = "++ indent' (unQ q)++" }"
   where indent' = unlines . (\s -> case s of
                                      [] -> []
                                      (h:t) -> h : (map (spaces ++) t)) . lines . show
         spaces = replicate 10 ' '

-- Smart constructors for NullView
notNull :: NullView
notNull = []

-- Shorthand for combining a preTag and a postTag
-- preTags :: Maybe Tag -> Maybe Tag -> TagList
-- preTags a b = promote a `mappend` promote b
--   where promote = maybe [] (\x -> [(x,PreUpdate TagTask)])

promotePreTag :: HandleTag -> TagList
promotePreTag = maybe [] (\x -> [(x,PreUpdate TagTask)]) . apply

makeEmptyNullView :: HandleTag -> HandleTag -> NullView
makeEmptyNullView a b = [(mempty, promotePreTag a ++ promotePreTag b)]

makeTestNullView ::  TestInfo -> HandleTag -> HandleTag -> NullView
makeTestNullView (w,d) a b = [(SetTestInfo (Map.singleton w (Set.singleton d)), promotePreTag a ++ promotePreTag b)]

tagWrapNullView :: HandleTag -> HandleTag -> NullView -> NullView
tagWrapNullView a b oldNV =
  case (promotePreTag a, promotePreTag b) of
    ([],[]) -> oldNV
    (pre,post) -> do
      (oldTests,oldTasks) <- oldNV
      return (oldTests,pre++oldTasks++post)

-- For PGroup, need to prepend reset tasks before others in nullView
addGroupResetsToNullView :: [Tag] -> Tag -> NullView -> NullView
addGroupResetsToNullView groupResets groupSet nv = [ (test, prepend (append tags) ) | (test,tags) <- nv ]
  where prepend = foldr (\h t -> (h:).t) id . map (\tag->(tag,PreUpdate ResetGroupStopTask)) $ groupResets
        append = (++[(groupSet,PreUpdate SetGroupStopTask)])

-- For PStar, need to put in the orbit TagTasks
orbitWrapNullView :: Maybe Tag -> [Tag] -> NullView -> NullView
orbitWrapNullView mOrbit orbitResets oldNV =
  case (mOrbit,orbitResets) of
    (Nothing,[]) -> oldNV
    (Nothing,_) -> do (oldTests,oldTasks) <- oldNV
                      return (oldTests,prepend oldTasks)
    (Just o,_) -> do (oldTests,oldTasks) <- oldNV
                     return (oldTests,prepend $ [(o,PreUpdate EnterOrbitTask)] ++ oldTasks ++ [(o,PreUpdate LeaveOrbitTask)])
  where prepend = foldr (\h t -> (h:).t) id . map (\tag->(tag,PreUpdate ResetOrbitTask)) $ orbitResets

-- The NullViews are ordered, and later test sets that contain the
-- tests from any earlier entry will never be chosen.  This function
-- returns a list with these redundant elements removed.  Note that
-- the first unconditional entry in the list will be the last entry of
-- the returned list since the empty set is a subset of any other set.
cleanNullView :: NullView -> NullView
cleanNullView [] = []
cleanNullView (first@(SetTestInfo sti,_):rest) | Map.null sti = first : []  -- optimization
                                               | otherwise =
  first : cleanNullView (filter (not . (setTI `Set.isSubsetOf`) . Map.keysSet . getTests . fst) rest)
  where setTI = Map.keysSet sti

-- Ordered Sequence of two NullViews: all ordered combinations of tests and tags.
-- Order of <- s1 and <- s2 is deliberately chosen to maintain preference priority
mergeNullViews :: NullView -> NullView -> NullView
mergeNullViews s1 s2 = cleanNullView $ do
  (test1,tag1) <- s1
  (test2,tag2) <- s2
  return (mappend test1 test2,mappend tag1 tag2)
-- mergeNullViews = cleanNullView $ liftM2 (mappend *** mappend)

-- Concatenated two ranges of number of accepted characters
seqTake :: (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Maybe Int)
seqTake (x1,y1) (x2,y2) = (x1+x2,liftM2 (+) y1 y2)

-- Parallel combination of list of ranges of number of accepted characters
orTakes :: [(Int, Maybe Int)] -> (Int,Maybe Int)
orTakes [] = (0,Just 0)
orTakes ts = let (xs,ys) = unzip ts
             in (minimum xs, foldl1 (liftM2 max) ys)

-- Invariant: apply (toAdvice _ ) == mempty
apply :: HandleTag -> Maybe Tag
apply (Apply tag) = Just tag
apply _ = Nothing
toAdvice :: HandleTag -> HandleTag
toAdvice (Apply tag) = Advice tag
toAdvice s = s
noTag :: HandleTag -> Bool
noTag NoTag = True
noTag _ = False
fromHandleTag :: HandleTag -> Tag
fromHandleTag (Apply tag) = tag
fromHandleTag (Advice tag) = tag
fromHandleTag _ = error "fromHandleTag"

-- Predicates on the range of number of accepted  characters
varies :: Q -> Bool
varies Q {takes = (_,Nothing)} = True
varies Q {takes = (x,Just y)} = x/=y

mustAccept :: Q -> Bool
mustAccept q = (0/=) . fst . takes $ q

canAccept :: Q -> Bool
canAccept q = maybe True (0/=) $ snd . takes $ q

cannotAccept :: Q -> Bool
cannotAccept q = maybe False (0==) $ snd . takes $ q

-- This converts then input Pattern to an analyzed Q structure with
-- the tags assigned.
--
-- The analysis is filled in by a depth first search and the tags are
-- created top down and passed to children.  Thus information flows up
-- from the dfs of the children and simultaneously down in the form of
-- pre and post HandleTag data.  This bidirectional flow is handled
-- declaratively by using the MonadFix (i.e. mdo).
--
-- Invariant: A tag should exist in Q in exactly one place (and will
-- be in a preTag,postTag, or getOrbit field).  This is partly because
-- PGroup needs to know the tags are around precisely the expression
-- that it wants to record.  If the same tag were in other branches
-- then this would no longer be true.  The tag may or may not also
-- show up in one or more preReset list or resetOrbits list.
--
-- This invariant is enforced by each node either taking
-- responsibility (apply) for a passed in / created tag or sending it
-- to exactly one child node.  Other child nodes need to receive it
-- via toAdvice.  Leaf nodes are forced to apply any passed tags.
--
-- There is a final "qwin of Q {postTag=ISet.singleton 1}" and an
-- implied initial index tag of 0.
--
-- favoring pushing Apply into the child postTag makes PGroup happier

type PM = RWS (Maybe GroupIndex) [Either Tag GroupInfo] ([OP]->[OP],Tag)
type HHQ = HandleTag  -- m1 : info about left boundary / preTag
        -> HandleTag  -- m2 : info about right boundary / postTag
        -> PM Q

-- There is no group 0 here, since it is always the whole match and has no parent of its own
makeGroupArray :: GroupIndex -> [GroupInfo] -> Array GroupIndex [GroupInfo]
makeGroupArray maxGroupIndex groups = accumArray (\earlier later -> later:earlier) [] (1,maxGroupIndex) filler
    where filler = map (\gi -> (thisIndex gi,gi)) groups

-- Partial function: assumes starTrans has been run on the Pattern
-- Note that the lazy dependency chain for this very zigzag:
--   varies information is sent up the tree
--   handle tags depend on that and sends m1 m2 down the tree
--     makeGroup sends some tags to the writer (Right _)
--     withParent listens to children send group info to writer
--       and lazily looks resetGroupTags from aGroups, the result of all writer (Right _)
--       preReset stores the resetGroupTags result of the lookup in the tree
--     makeOrbit sends some tags to the writer (Left _)
--     withOrbit listens to children send orbit info to writer for resetOrbitTags
--   nullQ depends m1 m2 and resetOrbitTags and resetGroupTags and is sent up the tree
patternToQ :: CompOption -> (Pattern,(GroupIndex,DoPa)) -> (Q,Array Tag OP,Array GroupIndex [GroupInfo])
patternToQ compOpt (pOrig,(maxGroupIndex,_)) = (tnfa,aTags,aGroups) where
  (tnfa,(tag_dlist,nextTag),groups) = runRWS monad startReader startState
  aTags = listArray (0,pred nextTag) (tag_dlist [])
  aGroups = makeGroupArray maxGroupIndex (rights groups)

  -- implicitly inside a PGroup 0 converted into a GroupInfo 0 undefined 0 1
  monad = go (starTrans pOrig) (Advice 0) (Advice 1)
  -- startReader is accessed by getParentIndex and changed by nonCapture and withParent
  startReader :: Maybe GroupIndex
  startReader = Just 0                           -- start inside group 0, capturing enabled
  -- The startState is only acted upon in the "uniq" command
  -- Tag 0 is Minimized and Tag 1 is maximized, next tag has value of 2
  -- This is regardless of right or left associativity
  startState :: ([OP]->[OP],Tag)
  startState = ( (Minimize:) . (Maximize:) , 2)

  -- uniq uses MonadState and always returns an "Apply _" tag
  {-# INLINE uniq #-}
  uniq :: String -> PM HandleTag
  uniq _msg = fmap Apply (uniq' Maximize)
--  uniq _msg = do x <- fmap Apply (uniq' Maximize)
--                trace ('\n':msg ++ " Maximize "++show x) $ return x
--                return x

  ignore :: String -> PM Tag
  ignore _msg = uniq' Ignore
--  ignore _msg = do x <- uniq' Ignore
--                  trace ('\n':msg ++ " Ignore "++show x) $ return x
--                  return x

  {-# NOINLINE uniq' #-}
  uniq' :: OP -> PM Tag
  uniq' newOp = do
    (op,s) <- get                -- generate the next tag with bias newOp
    let op' = op . (newOp:)
        s' = succ s
    put $! (op',s')
    return s

  {-# INLINE makeOrbit #-}
  -- Specialize the monad operations and give more meaningful names
  -- makeOrbit uses MonadState(uniq) and MonadWriter(tell/Left)
  makeOrbit :: PM (Maybe Tag)
  makeOrbit = do x <- uniq' Orbit
--                 trace ('\n':"PStar Orbit "++show x) $ do
                 tell [Left x]
                 return (Just x)

  {-# INLINE withOrbit #-}
  -- withOrbit uses MonadWriter(listens to makeOrbit/Left), collects
  -- children at all depths
  withOrbit :: PM a -> PM (a,[Tag])
  withOrbit = listens $ fst . partitionEithers

  {-# INLINE makeGroup #-}
  -- makeGroup usesMonadWriter(tell/Right)
  makeGroup :: GroupInfo -> PM ()
  makeGroup = tell . (:[]) . Right

  {-# INLINE getParentIndex #-}
  -- getParentIndex uses MonadReader(ask)
  getParentIndex :: PM (Maybe GroupIndex)
  getParentIndex = ask

  {-# INLINE nonCapture #-}
  -- nonCapture uses MonadReader(local) to suppress getParentIndex to return Nothing
  nonCapture :: PM  a -> PM a
  nonCapture = local (const Nothing)

  -- withParent uses MonadReader(local) to set getParentIndex to return (Just this)
  -- withParent uses MonadWriter(listens to makeGroup/Right) to return contained group indices (stopTag)
  -- withParent is only safe if getParentIndex has been checked to be not equal to Nothing (see PGroup below)
  -- Note use of laziness: the immediate children's group index is used to look up all copies of the
  -- group in aGroups, including copies that are not immediate children.
  withParent :: GroupIndex -> PM a -> PM (a,[Tag])
  withParent this = local (const (Just this)) . listens childGroupInfo
    where childGroupInfo x =
            let gs = snd $ partitionEithers x
                children :: [GroupIndex]
                children = norep . sort . map thisIndex
                           -- filter to get only immediate children (efficiency)
                           . filter ((this==).parentIndex) $ gs
            in concatMap (map flagTag . (aGroups!)) (this:children)

  -- combineConcat is a partial function: Must not pass in an empty list
  -- Policy choices:
  --  * pass tags to apply to children and have no preTag or postTag here (so none added to nullQ)
  --  * middle 'mid' tag: give to left/front child as postTag so a Group there might claim it as a stopTag
  --  * if parent is Group then preReset will become non-empty
  combineConcat :: [Pattern] -> HHQ
  combineConcat | rightAssoc compOpt = foldr1 combineSeq . map go
                | otherwise          = foldl1 combineSeq . map go -- libtre default
    where {-# INLINE front'end #-}
          front'end | rightAssoc compOpt = liftM2 (,)
                    | otherwise = flip (liftM2 (flip (,)))
          combineSeq :: HHQ -> HHQ -> HHQ
          combineSeq pFront pEnd = (\ m1 m2 -> mdo
            let bothVary = varies qFront && varies qEnd
            a <- if noTag m1 && bothVary then uniq "combineSeq start" else return m1
            b <- if noTag m2 && bothVary then uniq "combineSeq stop" else return m2
            mid <- case (noTag a,canAccept qFront,noTag b,canAccept qEnd) of
                     (False,False,_,_) -> return (toAdvice a)
                     (_,_,False,False) -> return (toAdvice b)
                     _ -> if tagged qFront || tagged qEnd then uniq "combineSeq mid" else return NoTag
      --      qFront <- pFront a mid
      --      qEnd <- pEnd (toAdvice mid) b
            (qFront,qEnd) <- front'end (pFront a mid) (pEnd (toAdvice mid) b)
            -- XXX: Perhaps a "produces" should be created to compliment "wants",
            -- then "produces qEnd" could be compared to "wants qFront"
            let wanted = if WantsEither == wants qEnd then wants qFront else wants qEnd
            return $ Q { nullQ = mergeNullViews (nullQ qFront) (nullQ qEnd)
                             , takes = seqTake (takes qFront) (takes qEnd)
                             , preReset = [], postSet = [], preTag = Nothing, postTag = Nothing
                             , tagged = bothVary
                             , childGroups = childGroups qFront || childGroups qEnd
                             , wants = wanted
                             , unQ = Seq qFront qEnd }
                                   )
  go :: Pattern -> HHQ
  go pIn m1 m2 =
    let die = error $ "patternToQ cannot handle "++show pIn
        nil = return $ Q {nullQ=makeEmptyNullView m1 m2
                         ,takes=(0,Just 0)
                         ,preReset=[],postSet=[],preTag=apply m1,postTag=apply m2
                         ,tagged=False,childGroups=False,wants=WantsEither
                         ,unQ=Empty}
        one = return $ Q {nullQ=notNull
                         ,takes=(1,Just 1)
                         ,preReset=[],postSet=[],preTag=apply m1,postTag=apply m2
                         ,tagged=False,childGroups=False,wants=WantsQNFA
                         ,unQ = OneChar pIn}
        test myTest = return $ Q {nullQ=makeTestNullView myTest m1 m2
                                 ,takes=(0,Just 0)
                                 ,preReset=[],postSet=[],preTag=apply m1,postTag=apply m2
                                 ,tagged=False,childGroups=False,wants=WantsQT
                                 ,unQ=Test myTest }
        xtra = newSyntax compOpt
    in case pIn of
         PEmpty -> nil
         POr [] -> nil
         POr [branch] -> go branch m1 m2
         POr branches -> mdo
           -- 2009 : The PNonEmpty p as POr [PEmpty,p] takes no branch tracking tag.
           --        I claim this is because only accepting branches need tags,
           --        and the last accepting branch does not need a tag.
           --        Non-accepting possibilities can all commute to the front and
           --        become part of the nullQ.  The accepting bits then need prioritizing.
           --    Does the above require changes in POr handling in TNFA?  Yes.
           --    Have to always use nullQ instead of "recapitulating" it.
           --    Could also create a constant-writing tag instead of many index tags.
           -- Exasperation: This POr recursive mdo is very easy to make loop and lockup the program
           -- if needTags is False then there is no way to disambiguate branches so fewer tags are needed
           let needUniqTags = childGroups ans
           let needTags = varies ans || childGroups ans -- childGroups detects that "abc|a(b)c" needs tags
           a <- if noTag m1 && needTags then uniq "POr start" else return m1 -- whole POr
           b <- if noTag m2 && needTags then uniq "POr stop" else return m2 -- whole POr
           let aAdvice = toAdvice a -- all branches share 'aAdvice'
               bAdvice = toAdvice b -- last branch gets 'bAdvice', others may get own tag
               -- Due to the recursive-do, it seems that I have to put the if needTags into the op'
               newUniq = if needUniqTags then uniq "POr branch" else return bAdvice
--           trace ("\nPOr sub "++show aAdvice++" "++show bAdvice++"needsTags is "++show needTags) $ return ()
           -- The "bs" values are allocated in left-to-right order before the children in "qs"
           -- optimization: low priority for last branch is implicit, do not create separate tag here.
           bs <- fmap (++[bAdvice]) $ replicateM (pred $ length branches) newUniq -- 2 <= length ps
           -- create all the child branches in left-to-right order after the "bs"
           qs <- forM (zip branches bs) (\(branch,bTag) ->  (go branch aAdvice bTag))
           let wqs = map wants qs
               wanted = if any (WantsBoth==) wqs then WantsBoth
                          else case (any (WantsQNFA==) wqs,any (WantsQT==) wqs) of
                                 (True,True) -> WantsBoth
                                 (True,False) -> WantsQNFA
                                 (False,True) -> WantsQT
                                 (False,False) -> WantsEither
               nullView = cleanNullView . tagWrapNullView a b . concatMap nullQ $ qs
               -- The nullView computed above takes the nullQ of the branches and combines them.  This
               -- assumes that the pre/post tags of the children are also part of the nullQ values.  So
               -- for consistency, POr must then add its own pre/post tags to its nullQ value.  Note that
               -- concatMap sets the left-to-right preference when choosing the null views.
           let ans = Q { nullQ = nullView
                       , takes = orTakes . map takes $ qs
                       , preReset = [], postSet = []
                       , preTag = apply a, postTag = apply b
                       , tagged = needTags
                       , childGroups = any childGroups qs
                       , wants = wanted
                       , unQ = Or qs }
           return ans
         PConcat [] -> nil -- fatal to pass [] to combineConcat
         PConcat ps -> combineConcat ps m1 m2
         PStar mayFirstBeNull p -> mdo
           let accepts    = canAccept q
               -- if needsOrbit is False then there is no need to disambiguate captures on each orbit
               -- Both checks are useful because (varies q) of True does not imply (childGroups q) of True when under PNonCapture
               needsOrbit = varies q && childGroups q
               -- if needsOrbit then must check start/stop before the Orbit tag
               -- if accepts then must check start/stop of whole pattern
               needsTags  = needsOrbit || accepts       -- important that needsOrbit implies needsTags
           a <- if noTag m1 && needsTags then uniq "PStar start" else return m1
           b <- if noTag m2 && needsTags then uniq "PStar stop" else return m2
           mOrbit <- if needsOrbit then makeOrbit else return Nothing -- any Orbit tag is created after the pre and post tags
--           test1 <- if tagged q then uniq "not-TEST1" Minimize else return NoTag
-- XXX XXX 1.1.5 testing second NoTag replaced with (toAdvice b)
           (q,resetOrbitTags) <- withOrbit (go p NoTag (toAdvice b)) -- all contained orbit tags get listened to (not including this one).
           let nullView | mayFirstBeNull = cleanNullView $ childViews ++ skipView
                        | otherwise = skipView
                 where childViews = tagWrapNullView a b . orbitWrapNullView mOrbit resetOrbitTags $ nullQ q
                       skipView = makeEmptyNullView a b
           return $ Q { nullQ = nullView
                      , takes = (0,if accepts then Nothing else (Just 0))
                      , preReset = [], postSet = []
                      , preTag = apply a, postTag = apply b
                      , tagged = needsTags
                      , childGroups = childGroups q
                      , wants = WantsQT
                      , unQ =Star { getOrbit = mOrbit
                                  , resetOrbits = resetOrbitTags
                                  , firstNull = mayFirstBeNull
                                  , unStar = q } }
         PCarat dopa -> test (Test_BOL,dopa)
         PDollar dopa -> test (Test_EOL,dopa)
         PChar {} -> one
         PDot {} -> one
         PAny {} -> one
         PAnyNot {} -> one
         -- CompOption's newSyntax enables these escaped anchors
         PEscape dopa '`'  | xtra -> test (Test_BOB,dopa)
         PEscape dopa '\'' | xtra -> test (Test_EOB,dopa)
         PEscape dopa '<'  | xtra -> test (Test_BOW,dopa)
         PEscape dopa '>'  | xtra -> test (Test_EOW,dopa)
         PEscape dopa 'b'  | xtra -> test (Test_EdgeWord,dopa)
         PEscape dopa 'B'  | xtra -> test (Test_NotEdgeWord,dopa)
         -- otherwise escape codes are just the escaped character
         PEscape {} -> one

         -- A PGroup node in the Pattern tree does not become a node
         -- in the Q/P tree. A PGroup can share and pass along a
         -- preTag (with Advice) with other branches, but will pass
         -- down an Apply postTag.
         --
         -- If the parent index is Nothing then this is part of a
         -- non-capturing subtree and ignored.  This is a lazy and
         -- efficient alternative to rebuilding the tree with PGroup
         -- Nothing replacing PGroup (Just _).
         --
         -- Guarded by the getParentIndex /= Nothing check is the
         -- withParent command.
         --
         PGroup Nothing p -> go p m1 m2
         PGroup (Just this) p -> do
           mParent <- getParentIndex
           case mParent of
             Nothing -> go p m1 m2 -- just like PGroup Nothing p
             Just parent -> do
               -- 'a' may be Advice or Apply from parent or Apply created here
               a <- if noTag m1 then uniq "PGroup start" else return m1
               b <- if noTag m2 then uniq "PGroup stop" else return m2
               flag <- ignore "PGroup ignore"
{-
               -- 'b' may be Apply from parent or Apply created here
               b <- if isNothing (apply m2) then uniq "PGroup" else return m2
-}
               (q,resetGroupTags) <- withParent this (go p a b)  -- all immediate child groups stop tags get listened to.
               -- 2009: makeGroup performs a tell, why after withParent? I am no longer sure.
               makeGroup (GroupInfo this parent (fromHandleTag a) (fromHandleTag b) flag)
               return $ q { nullQ = addGroupResetsToNullView resetGroupTags flag (nullQ q)
                          , tagged = True
                          , childGroups = True
                          , preReset = resetGroupTags `mappend` (preReset q)
                          , postSet = (postSet q) `mappend` [flag]
                          }

         -- A PNonCapture node in the Pattern tree does not become a
         -- node in the Q/P tree.  It sets the parent to Nothing while
         -- processing the sub-tree.
         PNonCapture p -> nonCapture (go p m1 m2)

         -- these are here for completeness of the case branches, currently starTrans replaces them all
         PPlus {} -> die
         PQuest {} -> die
         PBound {} -> die
         -- PNonEmpty is deprecated, and not produced in Pattern by starTrans anymore
         PNonEmpty {} -> die

{-
Similar to change in WinTags for QT/QNFA:
Change the NullView to use a tasktags instead of wintags since they are all PreUpdate

         -- PNonEmpty means the child pattern p can be skipped by
         -- bypassing the pattern.  This is only used in the case p
         -- can accept 0 and can accept more than zero characters
         -- (thus the assertions, enforced by CorePattern.starTrans).
         -- The important thing about this case is intercept the
         -- "accept 0" possibility and replace with "skip".
         PNonEmpty p -> mdo
           let needsTags = canAccept q
           a <- if noTag m1 && needsTags then uniq Minimize else return m1
           b <- if noTag m2 && needsTags then uniq Maximize else return m2
           q <- go p (toAdvice a) (toAdvice b)
           when (not needsTags) (err $ "PNonEmpty could not accept characters: "++show (p,pOrig))
           when (mustAccept q) (err $ "patternToQ : PNonEmpty provided with a *mustAccept* pattern: "++show (p,pOrig))
           return $ Q { nullQ = emptyNull (preTags (apply a) (apply b)) -- The meaning of NonEmpty
                      , takes = (0,snd (takes q))                       -- like Or, drop lower bound to 0
                      , preReset = []
                      , preTag = apply a, postTag = apply b             -- own the closing tag so it will not end a PGroup
                      , tagged = needsTags
                      , childGroups = childGroups q
                      , wants = wants q  -- the test case is "x" =~ "(.|$){1,3}"
                      , unQ = NonEmpty q }

-}
{-
emptyNull :: TagList -> NullView
emptyNull tags = (mempty, tags) : []

testNull :: TestInfo -> TagList -> NullView
testNull (w,d) tags = (SetTestInfo (Map.singleton w (Set.singleton d)), tags) : []

-- Prepend tags to nullView
addTagsToNullView :: TagList -> NullView -> NullView
addTagsToNullView [] oldNV = oldNV
addTagsToNullView tags oldNV= do
  (oldTest,oldTags) <- oldNV
  return (oldTest,tags `mappend` oldTags)

-}


-- xxx todo
--
-- see of PNonEmpty -> NonEmpty -> TNFA is really smarter than POr about tags
