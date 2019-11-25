-- XXX design uncertainty:  should preResets be inserted into nullView?
-- if not, why not? ADDED

-- XXX design uncertainty: what does act -> actNullable ->
-- actNullableTagless not use nullQ and same for inStar, etc?
-- TODO : try rewriting whole qToNFA in terms of "act"
-- (That will require re-organizing the continuation data a bit)

-- | "Text.Regex.TDFA.TNFA" converts the CorePattern Q\/P data (and its
-- Pattern leafs) to a QNFA tagged non-deterministic finite automata.
-- 
-- This holds every possible way to follow one state by another, while
-- in the DFA these will be reduced by picking a single best
-- transition for each (soure,destination) pair.  The transitions are
-- heavily and often redundantly annotated with tasks to perform, and
-- this redundancy is reduced when picking the best transition.  So
-- far, keeping all this information has helped fix bugs in both the
-- design and implementation.
--
-- The QNFA for a Pattern with a starTraned Q\/P form with N one
-- character accepting leaves has at most N+1 nodes.  These nodes
-- repesent the future choices after accepting a leaf.  The processing
-- of Or nodes often reduces this number by sharing at the end of the
-- different paths.  Turning off capturing while compiling the pattern
-- may (future extension) reduce this further for some patterns by
-- processing Star with optimizations.  This compact design also means
-- that tags are assigned not just to be updated before taking a
-- transition (PreUpdate) but also after the transition (PostUpdate).
-- 
-- Uses recursive do notation.

module Text.Regex.TDFA.TNFA(patternToNFA
                            ,QNFA(..),QT(..),QTrans,TagUpdate(..)) where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Control.Monad(when)
import Control.Monad.State(State,runState,execState,get,put,modify)
import Data.Array.IArray(Array,array)
import Data.Char(toLower,toUpper,isAlpha,ord)
import Data.List(foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap(toAscList,null,unionWith,singleton,fromList,fromDistinctAscList)
import Data.IntMap.CharMap2(CharMap(..))
import qualified Data.IntMap.CharMap2 as Map(null,singleton,map)
import qualified Data.IntMap.EnumMap2 as EMap(null,keysSet,assocs)
import Data.IntSet.EnumSet2(EnumSet)
import qualified Data.IntSet.EnumSet2 as Set(singleton,toList,insert)
import Data.Maybe(catMaybes,isNothing)
import Data.Monoid as Mon(Monoid(..))
import qualified Data.Set as S(Set,insert,toAscList,empty)

import Text.Regex.TDFA.Common(QT(..),QNFA(..),QTrans,TagTask(..),TagUpdate(..),DoPa(..)
                             ,CompOption(..)
                             ,Tag,TagTasks,TagList,Index,WinTags,GroupIndex,GroupInfo(..)
                             ,common_error,noWin,snd3,mapSnd)
import Text.Regex.TDFA.CorePattern(Q(..),P(..),OP(..),WhichTest,cleanNullView,NullView
                                  ,SetTestInfo(..),Wanted(..),TestInfo
                                  ,mustAccept,cannotAccept,patternToQ)
import Text.Regex.TDFA.Pattern(Pattern(..),PatternSet(..),unSEC,PatternSetCharacterClass(..))
--import Debug.Trace

ecart :: String -> a -> a
ecart _ = id

err :: String -> a
err t = common_error "Text.Regex.TDFA.TNFA" t

debug :: (Show a) => a -> s -> s
debug _ s = s

qtwin,qtlose :: QT
-- qtwin is the continuation after matching the whole pattern.  It has
-- no futher transitions and sets tag #1 to the current position.
qtwin = Simple {qt_win=[(1,PreUpdate TagTask)],qt_trans=mempty,qt_other=mempty}
-- qtlose is the continuation to nothing, used when ^ or $ tests fail.
qtlose = Simple {qt_win=mempty,qt_trans=mempty,qt_other=mempty}

patternToNFA :: CompOption
             -> (Pattern,(GroupIndex, DoPa))
             -> ((Index,Array Index QNFA)
                ,Array Tag OP
                ,Array GroupIndex [GroupInfo])
patternToNFA compOpt pattern =
  let (q,tags,groups) = patternToQ compOpt pattern
      msg = unlines [ show q ]
  in debug msg (qToNFA compOpt q,tags,groups)

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 
-- Query function on Q

nullable :: Q -> Bool
nullable = not . null . nullQ

notNullable :: Q -> Bool
notNullable = null . nullQ

-- This asks if the preferred (i.e. first) NullView has no tests.
maybeOnlyEmpty :: Q -> Maybe WinTags
maybeOnlyEmpty (Q {nullQ = ((SetTestInfo sti,tags):_)}) = if EMap.null sti then Just tags else Nothing
maybeOnlyEmpty _ = Nothing

usesQNFA :: Q -> Bool
usesQNFA (Q {wants=WantsBoth}) = True
usesQNFA (Q {wants=WantsQNFA}) = True
usesQNFA _ = False

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 
-- Functions related to QT

-- dumb smart constructor used by qToQNFA
-- Possible: Go through the qt and keep only the best tagged transition(s) to each state to make simple NFA?
mkQNFA :: Index -> QT -> QNFA
mkQNFA i qt = debug ("\n>QNFA id="++show i) $
  QNFA i (debug ("\ngetting QT for "++show i) qt)

-- This uses the Eq QT instance above
-- ZZZ
mkTesting :: QT -> QT
mkTesting t@(Testing {qt_a=a,qt_b=b}) = if a==b then a else t -- Move to nfsToDFA XXX
mkTesting t = t

nullQT :: QT -> Bool
nullQT (Simple {qt_win=w,qt_trans=t,qt_other=o}) = noWin w && Map.null t && IMap.null o
nullQT _ = False

-- This reconstructs the set of tests checked in processing QT, adding
-- them to the passed set.
listTestInfo :: QT -> EnumSet WhichTest -> EnumSet WhichTest
listTestInfo qt s = execState (helper qt) s
  where helper (Simple {}) = return ()
        helper (Testing {qt_test = wt, qt_a = a, qt_b = b}) = do
          modify (Set.insert wt)
          helper a
          helper b

-- This is used to view "win" only through NullView, and is used in
-- processing Or.
applyNullViews :: NullView -> QT -> QT
applyNullViews [] win = win
applyNullViews nvs win = foldl' (dominate win) qtlose (reverse $ cleanNullView nvs) where

-- This is used to prefer to view "win" through NullView.  Losing is
-- replaced by the plain win.  This is employed by Star patterns to
-- express that the first iteration is allowed to match null, but
-- skipping the NullView occurs if the match fails.
preferNullViews :: NullView -> QT -> QT
preferNullViews [] win = win
preferNullViews nvs win = foldl' (dominate win) win (reverse $ cleanNullView nvs) where

{- 
dominate is common to applyNullViews and preferNullViews above.

Even I no longer understand it without study.

Oversimplified: The last argument has a new set of tests "sti" that
must be satisfied to then apply the new "tags" and reach the "win" QT.
Failing any of this set of tests leads to the "lose" QT.

Closer: The "win" may already have some other set of tests leading to
various branches, this set is cached in winTests.  And the "lose" may
already have some other set of tests leading to various branches.  The
combination of "win" and "lose" and "sti" must check the union of
these tests, which is "allTests".

Detail: The merging is done by useTest, where the tests in sti divert
losing to a branch of "lose" and winning to a branch of "win".  Tests
not in sti are unchanged (but the losing DoPa index might be added).
-}
dominate :: QT -> QT -> (SetTestInfo,WinTags) -> QT
dominate win lose x@(SetTestInfo sti,tags) = debug ("dominate "++show x) $
  let -- The winning states are reached through the SetTag
      win' = prependTags' tags win
      -- get the SetTestInfo 
      winTests = listTestInfo win $ mempty
      allTests = (listTestInfo lose $ winTests) `mappend` (EMap.keysSet sti)
      -- The first and second arguments of useTest are sorted
      -- At all times the second argument of useTest is a subset of the first
      useTest _ [] w _ = w -- no more dominating tests to fail to choose lose, so just choose win
      useTest (aTest:tests) allD@((dTest,dopas):ds) w l =
        let (wA,wB,wD) = branches w
            (lA,lB,lD) = branches l
            branches qt@(Testing {}) | aTest==qt_test qt = (qt_a qt,qt_b qt,qt_dopas qt)
            branches qt = (qt,qt,mempty)
        in if aTest == dTest
             then Testing {qt_test = aTest
                          ,qt_dopas = (dopas `mappend` wD) `mappend` lD
                          ,qt_a = useTest tests ds wA lA
                          ,qt_b = lB}
             else Testing {qt_test = aTest
                          ,qt_dopas = wD `mappend` lD
                          ,qt_a = useTest tests allD wA lA
                          ,qt_b = useTest tests allD wB lB}
      useTest [] _ _  _ = err "This case in dominate.useText cannot happen: second argument would have to have been null and that is checked before this case"
  in useTest (Set.toList allTests) (EMap.assocs sti) win' lose

-- 'applyTest' is only used by addTest
-- 2009: maybe need to keep track of whether a change is actually made
-- (beyond DoPa tracking) to the QT.
applyTest :: TestInfo -> QT -> QT
applyTest (wt,dopa) qt | nullQT qt = qt
                       | otherwise = applyTest' qt where
  applyTest' :: QT -> QT
  applyTest' q@(Simple {}) =
    mkTesting $ Testing {qt_test = wt
                        ,qt_dopas = Set.singleton dopa
                        ,qt_a = q 
                        ,qt_b = qtlose}
  applyTest' q@(Testing {qt_test=wt'}) =
    case compare wt wt' of
      LT -> Testing {qt_test = wt
                    ,qt_dopas = Set.singleton dopa
                    ,qt_a = q
                    ,qt_b = qtlose}
      EQ -> q {qt_dopas = Set.insert dopa (qt_dopas q)
              ,qt_b = qtlose}
      GT -> q {qt_a = applyTest' (qt_a q)
              ,qt_b = applyTest' (qt_b q)}

-- Three ways to merge a pair of QT's varying how winning transitions
-- are handled.
--
-- mergeQT_2nd is used by the NonEmpty case and always discards the
-- first argument's win and uses the second argment's win.
--
-- mergeAltQT is used by the Or cases and is biased to the first
-- argument's winning transition, if present.
--
-- mergeQT is used by Star and mergeE and combines the winning
-- transitions (concatenating the instructions).
mergeQT_2nd,mergeAltQT,mergeQT :: QT -> QT -> QT
mergeQT_2nd q1 q2 | nullQT q1 = q2
                  | otherwise = mergeQTWith (\_ w2 -> w2) q1 q2

mergeAltQT q1 q2 | nullQT q1 = q2  -- prefer winning with w1 then with w2
                 | otherwise = mergeQTWith (\w1 w2 -> if noWin w1 then w2 else w1) q1 q2
mergeQT q1 q2 | nullQT q1 = q2  -- union wins
              | nullQT q2 = q1  -- union wins
              | otherwise = mergeQTWith mappend q1 q2 -- no preference, win with combined SetTag XXX is the wrong thing! "(.?)*"

-- This takes a function which implements a policy on mergining
-- winning transitions and then merges all the transitions.  It opens
-- the CharMap newtype for more efficient operation, then rewraps it.
mergeQTWith :: (WinTags -> WinTags -> WinTags) -> QT -> QT -> QT
mergeQTWith mergeWins = merge where
  merge :: QT -> QT -> QT
  merge (Simple w1 t1 o1) (Simple w2 t2 o2) =
    let w' = mergeWins w1 w2
        t' = fuseQTrans t1 o1 t2 o2
        o' = mergeQTrans o1 o2
    in Simple w' t' o'
  merge t1@(Testing _ _ a1 b1) s2@(Simple {}) = mkTesting $
    t1 {qt_a=(merge a1 s2), qt_b=(merge b1 s2)}
  merge s1@(Simple {}) t2@(Testing _ _ a2 b2) = mkTesting $
    t2 {qt_a=(merge s1 a2), qt_b=(merge s1 b2)}
  merge t1@(Testing wt1 ds1 a1 b1) t2@(Testing wt2 ds2 a2 b2) = mkTesting $
    case compare wt1 wt2 of
      LT -> t1 {qt_a=(merge a1 t2), qt_b=(merge b1 t2)}
      EQ -> Testing {qt_test = wt1 -- same as wt2
                    ,qt_dopas = mappend ds1 ds2
                    ,qt_a = merge a1 a2
                    ,qt_b = merge b1 b2}
      GT -> t2 {qt_a=(merge t1 a2), qt_b=(merge t1 b2)}

  fuseQTrans :: (CharMap QTrans) -> QTrans
             -> (CharMap QTrans) -> QTrans
             -> CharMap QTrans
  fuseQTrans (CharMap t1) o1 (CharMap t2) o2 = CharMap (IMap.fromDistinctAscList (fuse l1 l2)) where
    l1 = IMap.toAscList t1
    l2 = IMap.toAscList t2
    fuse [] y  = mapSnd (mergeQTrans o1) y
    fuse x  [] = mapSnd (mergeQTrans o2) x
    fuse x@((xc,xa):xs) y@((yc,ya):ys) =
      case compare xc yc of
        LT -> (xc,mergeQTrans xa o2) : fuse xs y
        EQ -> (xc,mergeQTrans xa ya) : fuse xs ys
        GT -> (yc,mergeQTrans o1 ya) : fuse x  ys

  mergeQTrans :: QTrans -> QTrans -> QTrans
  mergeQTrans = IMap.unionWith mappend

-- Note: There are no append* operations. There are only these
-- prepend* operations because things are only prepended to the future
-- continuation.  And the ordering is significant.

-- This is only used in inStar/nullable
prependPreTag :: Maybe Tag -> QT -> QT
prependPreTag Nothing qt = qt
prependPreTag (Just tag) qt = prependTags' [(tag,PreUpdate TagTask)] qt

prependGroupResets :: [Tag] -> QT -> QT
prependGroupResets [] qt = qt
prependGroupResets tags qt = prependTags' [(tag,PreUpdate ResetGroupStopTask)|tag<-tags] qt

prependTags' :: TagList -> QT -> QT
prependTags' []  qt = qt
prependTags' tcs' qt@(Testing {}) = qt { qt_a = prependTags' tcs' (qt_a qt)
                                       , qt_b = prependTags' tcs' (qt_b qt) }
prependTags' tcs' (Simple {qt_win=w,qt_trans=t,qt_other=o}) =
  Simple { qt_win = if noWin w then w else tcs' `mappend` w
         , qt_trans = Map.map prependQTrans t
         , qt_other = prependQTrans o }
  where prependQTrans = fmap (map (\(d,tcs) -> (d,tcs' `mappend` tcs)))

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 
-- define type S which is a State monad, this allows the creation of the uniq QNFA ids and storing the QNFA
-- in an ascending order difference list for later placement in an array.

-- Type of State monad used inside qToNFA
type S = State (Index                             -- Next available QNFA index
               ,[(Index,QNFA)]->[(Index,QNFA)])    -- DList of previous QNFAs

-- Type of continuation of the NFA, not much more complicated
type E = (TagTasks            -- Things to do before the Either QNFA QT
                              -- with OneChar these become PostUpdate otherwise they become PreUpdate
         ,Either QNFA QT)     -- The future, packaged in the best way

-- See documentation below before the 'act' function.  This is for use inside a Star pattern.
type ActCont = ( E                      -- The eLoop is the dangerous recursive reference to continuation
                                        -- future that loops while accepting zero more characters
               , Maybe E                -- This holds the safe non-zero-character accepting continuation
               , Maybe (TagTasks,QNFA)) -- optimized merger of the above, used only inside act, to avoid orphan QNFA id values

-- newQNFA is the only operation that actually uses the monad get and put operations
newQNFA :: String -> QT -> S QNFA
newQNFA s qt = do
  (thisI,oldQs) <- get
  let futureI = succ thisI in seq futureI $ debug (">newQNFA< "++s++" : "++show thisI) $ do
  let qnfa = mkQNFA thisI qt -- (strictQT qt) -- making strictQNFA kills test (1,11) ZZZ
  put $! (futureI, oldQs . ((thisI,qnfa):))
  return qnfa

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 
-- E related functions

fromQNFA :: QNFA -> E
fromQNFA qnfa = (mempty,Left qnfa)

fromQT :: QT -> E
fromQT qt = (mempty,Right qt)

-- Promises the output will match (_,Left _), used by Or cases when any branch wants a QNFA continuation
asQNFA :: String -> E -> S E
asQNFA _ x@(_,Left _) = return x
asQNFA s (tags,Right qt) = do qnfa <- newQNFA s qt      -- YYY Policy choice: leave the tags
                              return (tags, Left qnfa)

-- Convert continuation E into a QNFA, only done at "top level" by qToNFA to get unique start state
getQNFA :: String -> E -> S QNFA
getQNFA _ ([],Left qnfa) = return qnfa
getQNFA s (tags,Left qnfa) = newQNFA s (prependTags' (promoteTasks PreUpdate tags) (q_qt qnfa))
getQNFA s (tags,Right qt) = newQNFA s (prependTags' (promoteTasks PreUpdate tags) qt)

-- Extract the QT from the E
getQT :: E -> QT
getQT (tags,cont) = prependTags' (promoteTasks PreUpdate tags) (either q_qt id cont)

-- 2009: This looks realllly dodgy, since it can convert a QNFA/Testing to a QT/Testing
-- without actually achieving anything except adding a DoPa to the Testing.  A diagnostic
-- series of runs might be needed to decide if this ever creates orphan id numbers.
-- Then applyTest might need to keep track of whether it actually changes anything.
addTest :: TestInfo -> E -> E
addTest ti (tags,cont) = (tags, Right . applyTest ti . either q_qt id $ cont)

-- This is used only with PreUpdate and PostUpdate as the first argument.
promoteTasks :: (TagTask->TagUpdate) -> TagTasks -> TagList
promoteTasks promote tags = map (\(tag,task) -> (tag,promote task)) tags

-- only used in addWinTags
demoteTags :: TagList -> TagTasks
demoteTags = map helper
  where helper (tag,PreUpdate tt) = (tag,tt)
        helper (tag,PostUpdate tt) = (tag,tt)

-- This is polymorphic so addWinTags can be cute below
{-# INLINE addWinTags #-}
addWinTags :: WinTags -> (TagTasks,a) -> (TagTasks,a)
addWinTags wtags (tags,cont) = (demoteTags wtags `mappend` tags
                               ,cont)

{-# INLINE addTag' #-}
-- This is polymorphic so addTagAC can be cute below
addTag' :: Tag -> (TagTasks,a) -> (TagTasks,a)
addTag' tag (tags,cont) = ((tag,TagTask):tags
                          ,cont)

-- a Maybe version of addTag' above, specializing 'a' to Either QNFA QT
addTag :: Maybe Tag -> E -> E
addTag Nothing e = e
addTag (Just tag) e = addTag' tag e

{-# INLINE addGroupResets #-}
-- This is polymorphic so addGroupResetsAC can be cute below
addGroupResets :: (Show a) => [Tag] -> (TagTasks,a) -> (TagTasks,a)
addGroupResets [] x = x
addGroupResets tags (tags',cont) = (foldr (:) tags' . map (\tag -> (tag,ResetGroupStopTask)) $ tags
                                   ,cont)

addGroupSets :: (Show a) => [Tag] -> (TagTasks,a) -> (TagTasks,a)
addGroupSets [] x = x
addGroupSets tags (tags',cont) = (foldr (:) tags' . map (\tag -> (tag,SetGroupStopTask)) $ tags
                                 ,cont)

-- Consume an ActCont.  Uses the mergeQT form to combine non-accepting
-- and accepting view of the continuation.
getE :: ActCont -> E
getE (_,_,Just (tags,qnfa)) = (tags, Left qnfa)  -- consume optimized mQNFA value returned by Star
getE (eLoop,Just accepting,_) = fromQT (mergeQT (getQT eLoop) (getQT accepting))
getE (eLoop,Nothing,_) = eLoop

-- 2009: See coment for addTest.  Here is a case where the third component might be a (Just qnfa) and it
-- is being lost even though the added test might be redundant.
addTestAC :: TestInfo -> ActCont -> ActCont
addTestAC ti (e,mE,_) = (addTest ti e
                        ,fmap (addTest ti) mE
                        ,Nothing)

-- These are AC versions of the add functions on E

addTagAC :: Maybe Tag -> ActCont -> ActCont
addTagAC Nothing ac = ac
addTagAC (Just tag) (e,mE,mQNFA) = (addTag' tag e
                                   ,fmap (addTag' tag) mE
                                   ,fmap (addTag' tag) mQNFA)

addGroupResetsAC :: [Tag] -> ActCont -> ActCont
addGroupResetsAC [] ac = ac
addGroupResetsAC tags (e,mE,mQNFA) = (addGroupResets tags e
                                     ,fmap (addGroupResets tags) mE
                                     ,fmap (addGroupResets tags) mQNFA)

addGroupSetsAC :: [Tag] -> ActCont -> ActCont
addGroupSetsAC [] ac = ac
addGroupSetsAC tags (e,mE,mQNFA) = (addGroupSets tags e
                                   ,fmap (addGroupSets tags) mE
                                   ,fmap (addGroupSets tags) mQNFA)

addWinTagsAC :: WinTags -> ActCont -> ActCont
addWinTagsAC wtags (e,mE,mQNFA) = (addWinTags wtags e
                                  ,fmap (addWinTags wtags) mE
                                  ,fmap (addWinTags wtags) mQNFA)
-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 

-- Initial preTag of 0th tag is implied. No other general pre-tags would be expected.
-- The qtwin contains the preTag of the 1st tag and is only set when a match is completed.
-- The fst Index is the index of the unique starting QNFA state.
-- The snd (Array Index QNFA) is all the QNFA states.
--
-- In the cases below, Empty is handled much like a Test with no TestInfo.
qToNFA :: CompOption -> Q -> (Index,Array Index QNFA)
qToNFA compOpt qTop = (q_id startingQNFA
                      ,array (0,pred lastIndex) (table [])) where
  -- Result startingQNFA is the top level's index
  -- State pair: fst 0 is the next state number (not yet used) going in, and lastIndex coming out (succ of last used)
  --             snd id is the difference list of states going in, and the finished list coming out
  (startingQNFA,(lastIndex,table)) =
    runState (getTrans qTop (fromQT $ qtwin) >>= getQNFA "top level") startState
  startState = (0,id)

  getTrans,getTransTagless :: Q -> E -> S E
  getTrans qIn@(Q {preReset=resets,postSet=sets,preTag=pre,postTag=post,unQ=pIn}) e = debug (">< getTrans "++show qIn++" <>") $
    case pIn of
      -- The case below is the ultimate consumer of every single OneChar in the input and the only caller of
      -- newTrans/acceptTrans which is the sole source of QT/Simple nodes.
      OneChar pat -> newTrans "getTrans/OneChar" resets pre pat . addTag post . addGroupSets sets $ e
      Empty -> return . addGroupResets resets . addTag pre . addTag post . addGroupSets sets $ e
      Test ti -> return . addGroupResets resets . addTag pre . addTest ti . addTag post . addGroupSets sets $ e
      _ -> return . addGroupResets resets . addTag pre =<< getTransTagless qIn (addTag post . addGroupSets sets $ e)

  getTransTagless qIn e = debug (">< getTransTagless "++show qIn++" <>") $
    case unQ qIn of
      Seq q1 q2 -> getTrans q1 =<< getTrans q2 e
      Or [] -> return e
      Or [q] -> getTrans q e
      Or qs -> do
        eqts <- if usesQNFA qIn
                  then do
                    eQNFA <- asQNFA "getTransTagless/Or/usesQNFA" e
                    sequence [ getTrans q eQNFA | q <- qs ]
                  else sequence [ getTrans q e | q <- qs ]
        let qts = map getQT eqts
        return (fromQT (foldr1 mergeAltQT qts))

      Star mOrbit resetTheseOrbits mayFirstBeNull q ->
        -- mOrbit of Just implies varies q and childGroups q
        let (e',clear) = -- debug ("\n>"++show e++"\n"++show q++"\n<") $
              if notNullable q then (e,True)  -- subpattern cannot be null
                else if null resetTheseOrbits && isNothing mOrbit
                       then case maybeOnlyEmpty q of
                              Just [] -> (e,True)    -- True because null of subpattern is same as skipping subpattern
                              Just tagList -> (addWinTags tagList e,False) -- null of subpattern NOT same as skipping
                              _ -> (fromQT . preferNullViews (nullQ q) . getQT $ e,False)  -- is NOT same as skipping
                       else (fromQT . resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit -- resetOrbitsQT and enterOrbitQT commute
                             . preferNullViews (nullQ q) . getQT . leaveOrbit mOrbit $ e,False)  -- perform resets when accepting 0 characters
        in if cannotAccept q then return e' else mdo
        mqt <- inStar q this
        (this,ans) <- case mqt of
                        Nothing -> err ("Weird pattern in getTransTagless/Star: " ++ show (qTop,qIn))
                        Just qt -> do
                          let qt' = resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit $ qt -- resetOrbitsQT and enterOrbitQT commute
                              thisQT = mergeQT qt' . getQT . leaveOrbit mOrbit $ e -- capture of subpattern or leave via next pattern (avoid null of subpattern on way out)
                              ansE = fromQT . mergeQT qt' . getQT $ e' -- capture of subpattern or leave via null of subpattern
                          thisE <- if usesQNFA q
                                  then return . fromQNFA =<< newQNFA "getTransTagless/Star" thisQT
                                  else return . fromQT $ thisQT
                          return (thisE,ansE)
        return (if mayFirstBeNull then (if clear then this  -- optimization to possibly preserve QNFA
                                                 else ans)
                  else this)

      {- NonEmpty is like actNullable (Or [Empty,q]) without the extra tag to prefer the first Empty branch -}
      NonEmpty q -> ecart ("\n> getTransTagless/NonEmpty"++show qIn)  $ do
        -- Assertion to check than Pattern.starTrans did its job right:
        when (cannotAccept q) (err $ "getTransTagless/NonEmpty : provided with a *cannotAccept* pattern: "++show (qTop,qIn))
        when (mustAccept q) (err $ "getTransTagless/NonEmpty : provided with a *mustAccept* pattern: "++show (qTop,qIn))
        let e' = case maybeOnlyEmpty qIn of
                   Just [] -> e
                   Just _wtags -> e -- addWinTags wtags e  XXX was duplicating tags
                   Nothing -> err $ "getTransTagless/NonEmpty is supposed to have an emptyNull nullView : "++show qIn
        mqt <- inStar q e
        return $ case mqt of
                   Nothing -> err ("Weird pattern in getTransTagless/NonEmpty: " ++ show (qTop,qIn))
                   Just qt -> fromQT . mergeQT_2nd qt . getQT $ e' -- ...and then this sets qt_win to exactly that of e'
      _ -> err ("This case in Text.Regex.TNFA.TNFA.getTransTagless cannot happen" ++ show (qTop,qIn))

  inStar,inStarNullableTagless :: Q -> E -> S (Maybe QT)
  inStar qIn@(Q {preReset=resets,postSet=sets,preTag=pre,postTag=post}) eLoop | notNullable qIn =
    debug (">< inStar/1 "++show qIn++" <>") $
    return . Just . getQT =<< getTrans qIn eLoop
                                                                 | otherwise =
    debug (">< inStar/2 "++show qIn++" <>") $
    return . fmap (prependGroupResets resets . prependPreTag pre) =<< inStarNullableTagless qIn (addTag post . addGroupSets sets $ eLoop)
    
  inStarNullableTagless qIn eLoop = debug (">< inStarNullableTagless "++show qIn++" <>") $ do
    case unQ qIn of
      Empty -> return Nothing -- with Or this discards () branch in "(^|foo|())*"
      Or [] -> return Nothing
      Or [q] -> inStar q eLoop
      Or qs -> do
        mqts <- if usesQNFA qIn
                  then do eQNFA <- asQNFA "inStarNullableTagless/Or/usesQNFA" eLoop
                          sequence [ inStar q eQNFA | q <- qs ]
                  else sequence [inStar q eLoop | q <- qs ]
        let qts = catMaybes mqts
            mqt = if null qts then Nothing else Just (foldr1 mergeAltQT qts)
        return mqt
      -- Calls to act are inlined by hand to actNullable.  This returns only cases where q1 or q2 or both
      -- accepted characters.  The zero-character case is handled by the tag wrapping by inStar.
      -- 2009: Does this look dodgy and repetitios of tags?  Seq by policy has no preTag or postTag.
      -- though it can have prependGroupResets, but those are not repeated in children so it is okay.
      Seq q1 q2 -> do (_,meAcceptingOut,_) <- actNullable q1 =<< actNullable q2 (eLoop,Nothing,Nothing)
                      return (fmap getQT meAcceptingOut)
      -- Calls to act are inlined by hand and are we losing the tags?
      Star {} -> do (_,meAcceptingOut,_) <- actNullableTagless qIn (eLoop,Nothing,Nothing)
                    return (fmap getQT meAcceptingOut)
      NonEmpty {} -> ecart ("\n> inStarNullableTagless/NonEmpty"++show qIn) $
                     do (_,meAcceptingOut,_) <- actNullableTagless qIn (eLoop,Nothing,Nothing)
                        return (fmap getQT meAcceptingOut)
      Test {} -> return Nothing -- with Or this discards ^ branch in "(^|foo|())*"
      OneChar {} -> err ("OneChar cannot have nullable True")

  {- act* functions

  These have a very complicated state that they receive and return as
  "the continuation".

   (E, Maybe E,Maybe (SetTag,QNFA))

  The first E is the source of the danger that must be avoided.  It
  starts out a reference to the QNFA/QT state that will be created by
  the most recent parent Star node.  Thus it is a recursive reference
  from the MonadFix machinery.  In particular, this value cannot be
  returned to the parent Star to be included in itself or we get a "let
  x = y; y=x" style infinite loop.

  As act* progresses the first E is actually modified to be the parent
  QNFA/QT as "seen" when all the elements to the right have accepted 0
  characters.  Thus it acquires tags and tests+tags (the NullView data
  is used for this purpose).

  The second item in the 3-tuple is a Maybe E.  This will be used as the
  source of the QT for this contents of the Star QNFA/QT.  It will be
  merged with the Star's own continuation data.  It starts out Nothing
  and stays that way as long as there are no accepting transitions in
  the Star's pattern.  This is value (via getQT) returned by inStar.

  The third item is a special optimization I added to remove a source
  of orphaned QNFAs.  A Star within Act will often have to create a
  QNFA node.  This cannot go into the second Maybe E item as Just
  (SetTag,Left QNFA) because this QNFA can have pulled values from the
  recursive parent Star's QNFA/QT in the first E value.  Thus pulling
  with getQT from the QNFA and using that as the Maybe E would likely
  cause an infinite loop.  This extra QNFA is stored in the thd3
  location for use by getE. To improve it further it can accumulate
  Tag information after being formed.

  When a non nullable Q is handled by act it checks to see if the
  third value is there, in which case it uses that QNFA as the total
  continuation (subsumed in getE).  Otherwise it merges the first E
  with any (Just E) in the second value to form the continuation.

  -}

  act :: Q -> ActCont -> S (Maybe E)
  act qIn c | nullable qIn = fmap snd3 $ actNullable qIn c
            | otherwise = debug (">< act "++show qIn++" <>") $ do
    mqt <- return . Just =<< getTrans qIn ( getE $ c )
    return mqt  -- or "return (fromQT qtlose,mqt,Nothing)"

  actNullable,actNullableTagless :: Q -> ActCont -> S ActCont
  actNullable qIn@(Q {preReset=resets,postSet=sets,preTag=pre,postTag=post,unQ=pIn}) ac =
    debug (">< actNullable "++show qIn++" <>") $ do
    case pIn of
      Empty -> return . addGroupResetsAC resets . addTagAC pre . addTagAC post . addGroupSetsAC sets $ ac
      Test ti -> return . addGroupResetsAC resets . addTagAC pre . addTestAC ti . addTagAC post . addGroupSetsAC sets $ ac
      OneChar {} -> err ("OneChar cannot have nullable True ")
      _ -> return . addGroupResetsAC resets . addTagAC pre =<< actNullableTagless qIn ( addTagAC post . addGroupSetsAC sets $ ac )

  actNullableTagless qIn ac@(eLoop,mAccepting,mQNFA) = debug (">< actNullableTagless "++show (qIn)++" <>") $ do
    case unQ qIn of
      Seq q1 q2 -> actNullable q1 =<< actNullable q2 ac   -- We know q1 and q2 are nullable
                      
      Or [] -> return ac
      Or [q] -> actNullableTagless q ac
      Or qs -> do
        cqts <- do
          if all nullable qs
            then sequence [fmap snd3 $ actNullable q ac | q <- qs]
            else do
              e' <- asQNFA "qToNFA/actNullableTagless/Or" . getE $ ac
              let act' :: Q -> S (Maybe E)
                  act' q = return . Just =<< getTrans q e'
              sequence [ if nullable q then fmap snd3 $ actNullable q ac else act' q | q <- qs ]
        let qts = map getQT (catMaybes cqts)
            eLoop' = case maybeOnlyEmpty qIn of
                       Just wtags -> addWinTags wtags eLoop -- nullable without tests; avoid getQT
                       Nothing -> fromQT $ applyNullViews (nullQ qIn) (getQT eLoop) -- suspect this of duplicating some tags with nullQ qIn
            mAccepting' = if null qts
                            then fmap (fromQT . applyNullViews (nullQ qIn) . getQT) mAccepting -- suspect this of duplicating some tags with nullQ qIn
                            else Just (fromQT $ foldr1 mergeAltQT qts)
            mQNFA' = if null qts
                       then case maybeOnlyEmpty qIn of
                              Just wtags -> fmap (addWinTags wtags) mQNFA
                              Nothing -> Nothing
                       else Nothing
        return (eLoop',mAccepting',mQNFA')

      Star mOrbit resetTheseOrbits mayFirstBeNull q -> do
        let (ac0@(_,mAccepting0,_),clear) =
              if notNullable q
                then (ac,True)
                else if null resetTheseOrbits && isNothing mOrbit
                       then case maybeOnlyEmpty q of
                              Just [] -> (ac,True)
                              Just wtags -> (addWinTagsAC wtags ac,False)
                              _ -> let nQ = fromQT . preferNullViews (nullQ q) . getQT
                                   in ((nQ eLoop,fmap nQ mAccepting,Nothing),False)
                       else let nQ = fromQT . resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit
                                     . preferNullViews (nullQ q) . getQT . leaveOrbit mOrbit
                            in ((nQ eLoop,fmap nQ mAccepting,Nothing),False)
        if cannotAccept q then return ac0 else mdo
          mChildAccepting <- act q (this,Nothing,Nothing)
          (thisAC@(this,_,_),ansAC) <- 
            case mChildAccepting of
              Nothing -> err $ "Weird pattern in getTransTagless/Star: " ++ show (qTop,qIn)
              Just childAccepting -> do
                let childQT = resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit . getQT $ childAccepting
                    thisQT = mergeQT childQT . getQT . leaveOrbit mOrbit . getE $ ac
                    thisAccepting =
                      case mAccepting of
                        Just futureAccepting -> Just . fromQT . mergeQT childQT . getQT $ futureAccepting
                        Nothing -> Just . fromQT $ childQT
                thisAll <- if usesQNFA q
                             then do thisQNFA <- newQNFA "actNullableTagless/Star" thisQT
                                     return (fromQNFA thisQNFA, thisAccepting, Just (mempty,thisQNFA))
                             else return (fromQT thisQT, thisAccepting, Nothing)
                let skipQT = mergeQT childQT . getQT . getE $ ac0  -- for first iteration the continuation uses NullView
                    skipAccepting =
                      case mAccepting0 of
                        Just futureAccepting0 -> Just . fromQT . mergeQT childQT . getQT $ futureAccepting0
                        Nothing -> Just . fromQT $ childQT
                    ansAll = (fromQT skipQT, skipAccepting, Nothing)
                return (thisAll,ansAll)
          return (if mayFirstBeNull then (if clear then thisAC else ansAC)
                    else thisAC)
      NonEmpty q -> ecart ("\n> actNullableTagless/NonEmpty"++show qIn) $ do
        -- We *know* that q is nullable from Pattern and CorePattern checks, but assert here anyway
        when (mustAccept q) (err $ "actNullableTagless/NonEmpty : provided with a *mustAccept* pattern: "++show (qTop,qIn))
        when (cannotAccept q) (err $ "actNullableTagless/NonEmpty : provided with a *cannotAccept* pattern: "++show (qTop,qIn))

        {- This is like actNullable (Or [Empty,q]) without the extra tag to prefer the first Empty branch -}
        let (clearE,_,_) = case maybeOnlyEmpty qIn of
                             Just [] -> ac
                             Just _wtags -> ac -- addWinTagsAC wtags ac XXX was duplicating tags
                             Nothing -> err $ "actNullableTagless/NonEmpty is supposed to have an emptyNull nullView : "++show (qTop,qIn)
        (_,mChildAccepting,_) <- actNullable q ac
        case mChildAccepting of
          Nothing -> err  $ "Weird pattern in actNullableTagless/NonEmpty: " ++ show (qTop,qIn)
            -- cannotAccept q checked for and excluded the above condition (and starTrans!)
          Just childAccepting -> do
            let childQT = getQT childAccepting
                thisAccepting = case mAccepting of
                                  Nothing -> Just . fromQT $ childQT
                                  Just futureAcceptingE -> Just . fromQT . mergeQT childQT . getQT $ futureAcceptingE
                                  -- I _think_ there is no need for mergeQT_2nd in the above.
            return (clearE,thisAccepting,Nothing)
      _ -> err $ "This case in Text.Regex.TNFA.TNFA.actNullableTagless cannot happen: "++show (qTop,qIn)

  -- This is applied directly to any qt immediately before passing to mergeQT
  resetOrbitsQT :: [Tag] -> QT -> QT
  resetOrbitsQT | lastStarGreedy compOpt = const id
                | otherwise = (\tags -> prependTags' [(tag,PreUpdate ResetOrbitTask)|tag<-tags])

  enterOrbitQT :: Maybe Tag -> QT -> QT
  enterOrbitQT | lastStarGreedy compOpt = const id
               | otherwise = maybe id (\tag->prependTags' [(tag,PreUpdate EnterOrbitTask)])

  leaveOrbit :: Maybe Tag -> E -> E
  leaveOrbit | lastStarGreedy compOpt = const id
             | otherwise = maybe id (\tag->(\(tags,cont)->((tag,LeaveOrbitTask):tags,cont)))

  -- 'newTrans' is the only place where PostUpdate is used and is only called from getTrans/OneChar
  --  and is the only caller of 'acceptTrans' to make QT/Simple nodes.
  newTrans :: String    -- debugging string for when a newQNFA is allocated
           -> [Tag]     -- which tags get ResetGroupStopTask in this transition (PreUpdate)
           -> Maybe Tag -- maybe one TagTask to update before incrementing the offset (PreUpdate)
           -> Pattern   -- the one character accepting Pattern of this transition
           -> E         -- the continuation state, reified to a QNFA, of after this Pattern
                       -- The fst part of the E is consumed here as a TagTask (PostUpdate)
           -> S E       -- the continuation state, as a QT, of before this Pattern
  newTrans s resets mPre pat (tags,cont) = do
    i <- case cont of
           Left qnfa -> return (q_id qnfa)     -- strictQNFA ZZZ no help
           Right qt -> do qnfa <- newQNFA s qt -- strictQT ZZZ no help
                          return (q_id qnfa)
    let post = promoteTasks PostUpdate tags
        pre  = promoteTasks PreUpdate ([(tag,ResetGroupStopTask) | tag<-resets] ++ maybe [] (\tag -> [(tag,TagTask)]) mPre)
    return . fromQT $ acceptTrans pre pat post i -- fromQT $ strictQT no help

  -- 'acceptTrans' is the sole creator of QT/Simple and is only called by getTrans/OneChar/newTrans
  acceptTrans :: TagList -> Pattern -> TagList -> Index -> QT
  acceptTrans pre pIn post i =
    let target = IMap.singleton i [(getDoPa pIn,pre++post)]
    in case pIn of
         PChar _ char ->
           let trans = toMap target [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PEscape _ char ->
           let trans = toMap target [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PDot _ -> Simple { qt_win = mempty, qt_trans = dotTrans, qt_other = target }
         PAny _ ps ->
           let trans = toMap target . S.toAscList . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PAnyNot _ ps ->
           let trans = toMap mempty . S.toAscList . addNewline . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = target }
         _ -> err ("Cannot acceptTrans pattern "++show (qTop,pIn))
    where  -- Take a common destination and a sorted list of unique chraceters
           -- and create a map from those characters to the common destination
      toMap :: IntMap [(DoPa,[(Tag, TagUpdate)])] -> [Char]
            -> CharMap (IntMap [(DoPa,[(Tag, TagUpdate)])])
      toMap dest | caseSensitive compOpt = CharMap . IMap.fromDistinctAscList . map (\c -> (ord c,dest))
                 | otherwise = CharMap . IMap.fromList . ($ []) 
                               . foldr (\c dl -> if isAlpha c
                                                   then (dl.((ord (toUpper c),dest):)
                                                           .((ord (toLower c),dest):)
                                                        )
                                                   else (dl.((ord c,dest):))
                                       ) id 
      addNewline | multiline compOpt = S.insert '\n'
                 | otherwise = id
      dotTrans | multiline compOpt = Map.singleton '\n' mempty
               | otherwise = Mon.mempty

{-

prepend architecture becomes
prependTags :: TagTask -> [Tag] -> QT -> QT
which always uses PreUpdate and the same task for all the tags

qt_win seems to only allow PreUpdate so why keep the same type?


ADD ORPHAN ID check and make this a fatal error while testing

-}

-- | decodePatternSet cannot handle collating element and treats
-- equivalence classes as just their definition and nothing more.
decodePatternSet :: PatternSet -> S.Set Char
decodePatternSet (PatternSet msc mscc _ msec) =
  let baseMSC = maybe S.empty id msc
      withMSCC = foldl (flip S.insert) baseMSC  (maybe [] (concatMap decodeCharacterClass . S.toAscList) mscc)
      withMSEC = foldl (flip S.insert) withMSCC (maybe [] (concatMap unSEC . S.toAscList) msec)
  in withMSEC

-- | This returns the disctince ascending list of characters
-- represented by [: :] values in legalCharacterClasses; unrecognized
-- class names return an empty string
decodeCharacterClass :: PatternSetCharacterClass -> String
decodeCharacterClass (PatternSetCharacterClass s) =
  case s of
    "alnum" -> ['0'..'9']++['a'..'z']++['A'..'Z']
    "digit" -> ['0'..'9']
    "punct" -> ['\33'..'\47']++['\58'..'\64']++['\91'..'\95']++"\96"++['\123'..'\126']
    "alpha" -> ['a'..'z']++['A'..'Z']
    "graph" -> ['\41'..'\126']
    "space" -> "\t\n\v\f\r "
    "blank" -> "\t "
    "lower" -> ['a'..'z']
    "upper" -> ['A'..'Z']
    "cntrl" -> ['\0'..'\31']++"\127" -- with NUL
    "print" -> ['\32'..'\126']
    "xdigit" -> ['0'..'9']++['a'..'f']++['A'..'F']
    "word" -> ['0'..'9']++['a'..'z']++['A'..'Z']++"_"
    _ -> []

{-
-- | This is the list of recognized [: :] character classes, others
-- are decoded as empty.
legalCharacterClasses :: [String]
legalCharacterClasses = ["alnum","digit","punct","alpha","graph"
  ,"space","blank","lower","upper","cntrl","print","xdigit","word"]

-}
