{-|
Module: Text.Regex.TDFA
Copyright: (c) Chris Kuklewicz 2007-2009
SPDX-License-Identifier: BSD-3-Clause
Maintainer: hvr@gnu.org

The "Text.Regex.TDFA" module provides a backend for regular
expressions. It provides instances for the classes defined and
documented in "Text.Regex.Base" and re-exported by this module.  If
you import this along with other backends then you should do so with
qualified imports (with renaming for convenience).

This regex-tdfa package implements, correctly, POSIX extended regular
expressions.  It is highly unlikely that the @regex-posix@ package on
your operating system is correct, see
<http://www.haskell.org/haskellwiki/Regex_Posix> for examples of your
OS's bugs.

= Importing and using

<https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-depends Declare a dependency> on the @regex-tdfa@ library in your @.cabal@ file:

> build-depends: regex-tdfa ^>= 1.3.1.0

In Haskell modules where you want to use regexes simply @import@ /this/ module:

@
import "Text.Regex.TDFA"
@

= Basics

@
λ> let emailRegex = "[a-zA-Z0-9+.\_-]+\@[a-zA-Z-]+\\\\.[a-z]+"
λ> "my email is email@email.com" '=~' emailRegex :: Bool
>>> True

/-- non-monadic/
λ> \<to-match-against\> '=~' \<regex\>

/-- monadic, uses 'fail' on lack of match/
λ> \<to-match-against\> '=~~' \<regex\>
@

('=~') and ('=~~') are polymorphic in their return type. This is so that
regex-tdfa can pick the most efficient way to give you your result based on
what you need. For instance, if all you want is to check whether the regex
matched or not, there's no need to allocate a result string. If you only want
the first match, rather than all the matches, then the matching engine can stop
after finding a single hit.

This does mean, though, that you may sometimes have to explicitly specify the
type you want, especially if you're trying things out at the REPL.

= Common use cases

== Get the first match

@
/-- returns empty string if no match/
a '=~' b :: String  /-- or ByteString, or Text.../

λ> "alexis-de-tocqueville" '=~' "[a-z]+" :: String
>>> "alexis"

λ> "alexis-de-tocqueville" '=~' "[0-9]+" :: String
>>> ""
@

== Check if it matched at all

@
a '=~' b :: Bool

λ> "alexis-de-tocqueville" '=~' "[a-z]+" :: Bool
>>> True
@

== Get first match + text before/after

@
/-- if no match, will just return whole/
/-- string in the first element of the tuple/
a =~ b :: (String, String, String)

λ> "alexis-de-tocqueville" '=~' "de" :: (String, String, String)
>>> ("alexis-", "de", "-tocqueville")

λ> "alexis-de-tocqueville" '=~' "kant" :: (String, String, String)
>>> ("alexis-de-tocqueville", "", "")
@

== Get first match + submatches

@
/-- same as above, but also returns a list of just submatches./
/-- submatch list is empty if regex doesn't match at all/
a '=~' b :: (String, String, String, [String])

λ> "div[attr=1234]" '=~' "div\\\\[([a-z]+)=([^]]+)\\\\]" :: (String, String, String, [String])
>>> ("", "div[attr=1234]", "", ["attr","1234"])
@

== Get /all/ matches

@
/-- can also return Data.Array instead of List/
'getAllTextMatches' (a '=~' b) :: [String]

λ> 'getAllTextMatches' ("john anne yifan" '=~' "[a-z]+") :: [String]
>>> ["john","anne","yifan"]
@

= Feature support

This package does provide captured parenthesized subexpressions.

Depending on the text being searched this package supports Unicode.
The @[Char]@, @Text@, @Text.Lazy@, and @(Seq Char)@ text types support Unicode.  The @ByteString@
and @ByteString.Lazy@ text types only support ASCII.  It is possible to
support utf8 encoded @ByteString.Lazy@ by using regex-tdfa and
<http://hackage.haskell.org/package/regex-tdfa-utf8 regex-tdfa-utf8>
packages together (required the utf8-string package).

As of version 1.1.1 the following GNU extensions are recognized, all
anchors:

* \\\` at beginning of entire text
* \\\' at end of entire text
* \\\< at beginning of word
* \\\> at end of word
* \\b at either beginning or end of word
* \\B at neither beginning nor end of word

The above are controlled by the 'newSyntax' Bool in 'CompOption'.

Where the "word" boundaries means between characters that are and are
not in the [:word:] character class which contains [a-zA-Z0-9_].  Note
that \\\< and \\b may match before the entire text and \\\> and \\b may
match at the end of the entire text.

There is no locale support, so collating elements like [.ch.] are
simply ignored and equivalence classes like [=a=] are converted to
just [a].  The character classes like [:alnum:] are supported over
ASCII only, valid classes are alnum, digit, punct, alpha, graph,
space, blank, lower, upper, cntrl, print, xdigit, word.

This package does not provide "basic" regular expressions.  This
package does not provide back references inside regular expressions.

The package does not provide Perl style regular expressions.  Please
look at the <http://hackage.haskell.org/package/regex-pcre regex-pcre>
and <http://hackage.haskell.org/package/pcre-light pcre-light> packages instead.

This package does not provide find-and-replace.

= Avoiding backslashes

If you find yourself writing a lot of regexes, take a look at
<http://hackage.haskell.org/package/raw-strings-qq raw-strings-qq>. It'll
let you write regexes without needing to escape all your backslashes.

@
\{\-\# LANGUAGE QuasiQuotes \#\-\}

import Text.RawString.QQ
import Text.Regex.TDFA

λ> "2 * (3 + 1) / 4" '=~' [r|\\([^)]+\\)|] :: String
>>> "(3 + 1)"
@

-}

module Text.Regex.TDFA(getVersion_Text_Regex_TDFA
                      ,(=~),(=~~)
                      ,module Text.Regex.TDFA.Common
                      ,module Text.Regex.Base) where

import qualified Control.Monad.Fail as Fail
import Data.Version(Version)
import Text.Regex.Base
import Text.Regex.TDFA.String()
import Text.Regex.TDFA.ByteString()
import Text.Regex.TDFA.ByteString.Lazy()
import Text.Regex.TDFA.Text()
import Text.Regex.TDFA.Text.Lazy()
import Text.Regex.TDFA.Sequence()
import Text.Regex.TDFA.Common(Regex,CompOption(..),ExecOption(..))
--import Text.Regex.TDFA.Wrap(Regex,CompOption(..),ExecOption(..),(=~),(=~~))

import Paths_regex_tdfa(version)

getVersion_Text_Regex_TDFA :: Version
getVersion_Text_Regex_TDFA = version


-- | This is the pure functional matching operator.  If the target
-- cannot be produced then some empty result will be returned.  If
-- there is an error in processing, then 'error' will be called.
(=~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
     => source1 -> source -> target
(=~) x r = let make :: RegexMaker Regex CompOption ExecOption a => a -> Regex
               make = makeRegex
           in match (make r) x

-- | This is the monadic matching operator.  If a single match fails,
-- then 'fail' will be called.
(=~~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target, Fail.MonadFail m)
      => source1 -> source -> m target
(=~~) x r = do let make :: (RegexMaker Regex CompOption ExecOption a, Fail.MonadFail m) => a -> m Regex
                   make = makeRegexM
               q <- make r
               matchM q x
