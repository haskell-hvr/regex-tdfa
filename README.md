# regex-tdfa

This is [`regex-tdfa`](http://hackage.haskell.org/package/regex-tdfa) which is a pure Haskell regular expression library (for POSIX extended regular expressions) originally written by Christopher Kuklewicz.

The name "tdfa" stands for Tagged-DFA.

## Getting started

### Importing and using

[Declare a dependency](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-depends) on the `regex-tdfa` library in your `.cabal` file:

```
build-depends: regex-tdfa ^>= 1.3.1
```

In Haskell modules where you need to use regexes `import` the respective `regex-tdfa` module:

```haskell
import Text.Regex.TDFA
```

### Basics

```haskell
λ> emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"
λ> "my email is email@email.com" =~ emailRegex :: Bool
>>> True

-- non-monadic
<to-match-against> =~ <regex>

-- monadic, uses 'fail' on lack of match
<to-match-against> =~~ <regex>
```

`(=~)` and `(=~~)` are polymorphic in their return type. This is so that
regex-tdfa can pick the most efficient way to give you your result based on
what you need. For instance, if all you want is to check whether the regex
matched or not, there's no need to allocate a result string. If you only want
the first match, rather than all the matches, then the matching engine can stop
after finding a single hit.

This does mean, though, that you may sometimes have to explicitly specify the
type you want, especially if you're trying things out at the REPL.

### Common use cases

#### Get the first match

```haskell
-- returns empty string if no match
a =~ b :: String  -- or ByteString, or Text...

λ> "alexis-de-tocqueville" =~ "[a-z]+" :: String
>>> "alexis"

λ> "alexis-de-tocqueville" =~ "[0-9]+" :: String
>>> ""
```

#### Check if it matched at all

```haskell
a =~ b :: Bool

λ> "alexis-de-tocqueville" =~ "[a-z]+" :: Bool
>>> True
```

#### Get first match + text before/after

```haskell
-- if no match, will just return whole
-- string in the first element of the tuple
a =~ b :: (String, String, String)

λ> "alexis-de-tocqueville" =~ "de" :: (String, String, String)
>>> ("alexis-", "de", "-tocqueville")

λ> "alexis-de-tocqueville" =~ "kant" :: (String, String, String)
>>> ("alexis-de-tocqueville", "", "")
```

#### Get first match + submatches

```haskell
-- same as above, but also returns a list of /just/ submatches
-- submatch list is empty if regex doesn't match at all
a =~ b :: (String, String, String, [String])

λ> "div[attr=1234]" =~ "div\\[([a-z]+)=([^]]+)\\]"
     :: (String, String, String, [String])
>>> ("", "div[attr=1234]", "", ["attr","1234"])
```

#### Get all non-overlapping matches

```haskell
-- can also return Data.Array instead of List
getAllTextMatches (a =~ b) :: [String]

λ> getAllTextMatches ("john anne yifan" =~ "[a-z]+") :: [String]
>>> ["john","anne","yifan"]

λ> getAllTextMatches ("0a0b0" =~ "0[a-z]0") :: [String]
>>> ["0a0"]
```
Note that `"0b0"` is not included in the result since it overlaps with `"0a0"`.

#### Special characters

`regex-tdfa` only supports a small set of special characters and is much less
featureful than some other regex engines you might be used to, such as PCRE.

* ``\` `` &mdash; Match start of entire text (similar to `^` in other regex engines)
* `\'` &mdash; Match end of entire text (similar to `$` in other regex engines)
* `\<` &mdash; Match beginning of word
* `\>` &mdash; Match end of word
* `\b` &mdash; Match beginning or end of word
* `\B` &mdash; Match neither beginning nor end of word

### Less common stuff

#### Get match indices

```haskell
-- can also return Data.Array instead of List
getAllMatches (a =~ b) :: [(Int, Int)]  -- (index, length)

λ> getAllMatches ("john anne yifan" =~ "[a-z]+") :: [(Int, Int)]
>>> [(0,4), (5,4), (10,5)]
``````

#### Get submatch indices

```haskell
-- match of __entire__ regex is first element, not first capture
-- can also return Data.Array instead of List
getAllSubmatches (a =~ b) :: [(Int, Int)]  -- (index, length)

λ> getAllSubmatches ("div[attr=1234]" =~ "div\\[([a-z]+)=([^]]+)\\]")
     :: [(Int, Int)]
>>> [(0,14), (4,4), (9,4)]
```

### Replacement

`regex-tdfa` does not provide find-and-replace.

## The relevant links

This documentation is also available in [Text.Regex.TDFA haddock](http://hackage.haskell.org/package/regex-tdfa-1.2.3.2/docs/Text-Regex-TDFA.html).

This was also documented at the [Haskell wiki](https://wiki.haskell.org/Regular_expressions#regex-tdfa).  The original Darcs repository was at [code.haskell.org](http://code.haskell.org/regex-tdfa/).  When not updated, this was forked and maintained by Roman Cheplyaka as [regex-tdfa-rc](http://hackage.haskell.org/package/regex-tdfa-rc).

Then the repository moved to <https://github.com/ChrisKuklewicz/regex-tdfa>, which was primarily maintained by [Artyom (neongreen)](https://github.com/neongreen).

Finally, maintainership was passed on again and the repository moved to its current location at <https://github.com/haskell-hvr/regex-tdfa>.

## Avoiding backslashes

If you find yourself writing a lot of regexes, take a look at
[raw-strings-qq](http://hackage.haskell.org/package/raw-strings-qq). It'll
let you write regexes without needing to escape all your backslashes.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Text.Regex.TDFA

λ> "2 * (3 + 1) / 4" =~ [r|\([^)]+\)|] :: String
>>> "(3 + 1)"
```

## Known bugs and infelicities

* Regexes with large character classes combined with `{m,n}` are very slow and memory-hungry ([#3][]).

  > An example of such a regex is `^[\x0020-\xD7FF]{1,255}$`.

* POSIX submatch semantics are broken in some rare cases ([#2][]).

[#2]: https://github.com/haskell-hvr/regex-tdfa/issues/2
[#3]: https://github.com/haskell-hvr/regex-tdfa/issues/3

## About this package

This was inspired by the algorithm (and Master's thesis) behind the regular expression library known as [TRE or libtre](https://github.com/laurikari/tre/).  This was created by Ville Laurikari and tackled the difficult issue of efficient sub-match capture for POSIX regular expressions.

By building on this thesis and adding a few more optimizations, regex-tdfa matching input text of length N should have O(N) runtime, and should have a maximum memory bounded by the pattern size that does not scale with N. It should do this while returning well defined (and correct) values for the parenthesized sub-matches.

Regardless of performance, nearly every single OS and Libra for POSIX regular expressions has bugs in sub-matches.  This was detailed on the [Regex POSIX Haskell wiki page](https://wiki.haskell.org/Regex_Posix), and can be demonstrated with the [regex-posix-unittest](http://hackage.haskell.org/package/regex-posix-unittest) suite of checks.  Test [regex-tdfa-unittest](http://hackage.haskell.org/package/regex-tdfa-unittest) should show regex-tdfa passing these same checks.  I owe my understanding of the correct behvior and many of these unit tests to Glenn Fowler at AT&T ("An Interpretation of the POSIX regex Standard").

## Other related packages

You can find several other related packages by searching for "tdfa" on [hackage](http://hackage.haskell.org/packages/search?terms=tdfa).

## Document notes

This was written 2016-04-30.
