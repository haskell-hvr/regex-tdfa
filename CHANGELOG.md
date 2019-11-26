See also http://pvp.haskell.org/faq

## 1.3.1.0

- Merge <http://hackage.haskell.org/package/regex-tdfa-text> into `regex-tdfa`; see <https://github.com/haskell-hvr/regex-tdfa/issues/4>.
- Don't inject `ghc-options: -O2` by default anymore (see #7 for rationale) and introduce `force-O2` cabal flag to control the injection of `ghc-options: -O2`.
  Note that you can conveniently control optimization levels on a per-package granularity via `cabal.project` files; see [cabal's user-guide](https://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project) for more details.

# 1.3.0

- Same as 1.2.3.3 release; see <https://github.com/ChrisKuklewicz/regex-tdfa/issues/29>.
- Compatibility with GHC 8.8 and regex-base-0.9.4 (h/t @asr).
- Turned `regex-tdfa-unittest` into a `regex-tdfa` testsuite.

----

### 1.2.3.3 (deprecated)

* Compatibility with GHC 8.8 and regex-base-0.9.4 (h/t @asr).
* Turned `regex-tdfa-unittest` into a `regex-tdfa` testsuite.

### 1.2.3.2

* Significantly improved documentation (h/t William Yao).

### 1.2.3.1

* Compatibility with `containers-0.6`.

## 1.2.3

* Added `Semigroup` instances for some types (h/t Herbert Valerio Riedel).

## 1.2.2

* New maintainer.
* Now we don't reexport the problematic `Show` instance for functions.

## 1.2.1

* Updated dependency versions.

# 1.2.0

"Almost ghc-7.8" with the array 0.4 changes for `Data.Array.Unsafe`

----

# 1.1.8

Make ghc-7.0.2 on platorm 2011.2.0.0.0 happy

# 1.1.7

fix url below

# 1.1.6

Fix bug preventing `[]] [-] [^]] [^-]` (thanks to Maxime Henrion)

# 1.1.5

try `needUniqTags` in `POr` in CorePattern.hs, try `toAdvice b` for `PStar child`

# 1.1.4

fixed

# 1.1.3

BROKEN after 100 characters the `compressOrbit` dies!

# 1.1.2

worked

# 1.1.1

add gnu escapes

# 1.1.0

NewDFA code working

# 1.0.7

make NewDFA directory and String_NC

# 1.0.6

try NewDFATest_SBS with `uncons`

# 1.0.5

use `uncons` on SBS

# 1.0.4

try repaired NewDFATest_SBS

* np13: try to improve readability with the `mm` combinator? Yes!
* np12: expand `o` in the case where `t` lookup get `Nothing`? Yes – this is the fix!?
* np11: break multi to not look at `o` and just return `True`? Yes !!!!
* np10: Peel off `CharMap`/`IntMap` and DFA/DT with pattern matching? No
* np9:  `INLINE` `endOf`? No
* np8:  np6 and `NOINLINE` `endOff`? No
* np7:  just return `True`? Fast
* np6:  comment out ans check? No
* np5:  comment out all `Multi0` code? No
* np4:  comment out all `Single0` and `Single` code? No
* np3:  `!off` the multi? No
* np2:  comment out all Testing code? No

# 1.0.3

try to alter `matchTest` to not have the `Bool` args? No

# 1.0.2

arg, the prof is fast and the normal slow!

# 1.0.1

add NewDFATest.hs

# 0.99.20

go to many vs single?

# 0.99.19

try for pre-comparison of orbit-logs!

# 0.99.18

try alternate lazy/strict strategy in NewDFA. Fix offset laziness.

# 0.99.17

radical removal of flag array and adding of `SetVal` to handle groups

# 0.99.16

performance? up to v15

# 0.99.15

get string with NewDFA testing, unit tests and 1000 random regex pass

# 0.99.14

start changing to the new real DFA

# 0.99.13

more cleanup

# 0.99.12

try to debug 0.99.11: fixed `updateWinner`

# 0.99.11

improve above fix and make stuff work better – HAS BUG, along with old TDFA!

# 0.99.10

fixed `((.?)*)*` patterns by changing `PStar nullView` when `mayFirstBeNull`

# 0.99.9

testing changing `bestTrans`/`chooseWith`/`choose` to include `enterOrbit`/`newFlags`/`(_,True)` info

# 0.99.8

testing changing `Maximize` to `Minimize` for `Tag`s, decide `(a*)*` is canonical problem

# 0.99.7

Use `(PGroup Nothing)` in `Pattern` to decompose `PBound`

# 0.99.6

change to nested `nonEmpty` calls for `PBound`

# 0.99.5

remove `PNonEmpty` constructor

# 0.99.4

tests `pnonempty' = \ p -> POr [ PEmpty, p ]` instead of `PNonEmpty`
