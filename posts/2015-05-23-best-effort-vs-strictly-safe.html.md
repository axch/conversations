---
title: Best Effort <em>vs</em> Strictly Safe
date: 2015/05/23
author: Alexey Radul
---

Possibly the greatest tension in the design of programming languages
occurs when encountering a user program that doesn't quite make sense.
The two coherent schools of thought on the subject are READMORE

a. The platform should make its _best effort_ to continue the
   computation---choose reasonable rules for resolving potential
   ambiguities, so as to avoid crashing the program until absolutely
   necessary.

b. The platform should be _strictly safe_---detect potential
   ambiguities as early as possible and force the programmer to
   clarify until crash-free execution can be guaranteed.

For example, what should a programming language do when the user
writes a program that might attempt to add a number to a string?
Haskell will give a type error when compiling the program, forcing the
programmer to adjust their code (possibly by inserting an explicit
conversion).[^defer-type-errors] Common Lisp will run the program, and
raise an exception if the program actually does attempt to add a
number to a string.[^condition] Perl will parse the string as a number
and proceed (some would say "blithely").[^perl]

[^defer-type-errors]: As of version 7.6, the Glasgow Haskell Compiler
has the `-fdefer-type-errors` flag, which does allow the programmer to
try to execute a program with such an inconsistency.  However, that
type error will cause a crash if the offending code path is entered,
even if it would not have gone though with the dubious addition.

[^condition]: The programmer can define a handler for such conditions,
which gives a variety of options for proceeding, including substituting
some value for the result of the addition and continuing from there.

[^perl]: If the whole string doesn't parse, Perl will take the longest
prefix that does.  If no prefix of the string is a number at all, Perl
will interpret the empty prefix as denoting 0.

Which way is best?  Disciples of the best effort school point out
that such systems tend to be more robust, continuing to work (somehow)
in unanticipated situations (including tolerating some bugs in their
own programming).  Disciples of the strictly safe school point out
that such systems tend to have fewer mistakes and more predictable
behavior.  Which kinds of programs are easier to modify and to build
upon?  The debate fills volumes.

Let's think about basic data types---algebraic data types in Haskell,
versus records in various Lisp dialects.  One major mental difference
between Haskell and Lisp, that seems to trip up people learning their
second of those, is that Haskell always requires explicit
conversions between different algebraic data types, even if they look
similar; whereas Lisp is perfectly happy to plug together any producer
with any consumer, and as long as the producer only happens to make
records of types the consumer can handle, everything will be fine.
[^implicit-union]

[^implicit-union]: In ADT-language, one can say that Lisp records are
product types, and describe Lisp as synthesizing sum types on the fly.
The thing that's interesting is that Lisp also supplies implicit
(partial) conversions between (synthesized) sum types with factors of
the same names.

There is an obvious cost to the Haskell way, which is needing to
write code like

    foo_to_bar :: Foo -> Bar
    foo_to_bar Foo1 = Bar1
    foo_to_bar Foo2 = Bar2

and use it all over the place.  This is tedious, but not particularly
error-prone, because the type system helps you get it right.

On the other hand, there is a major benefit to the Haskell way: If all
conversions and injections are explicit, then it is possible to
statically determine what type everything should have, which quickly
catches whole slews of common and irritating errors.  (And also serves as
compiler-checked documentation of program structure).

This benefit has a deep version, too: if in Haskell one tries to put a
`Foo'` into a container meant to hold only `Foo`, one will quickly get an
error message that points both to the guilty insertion and to the
point in the code that requires it to be a container of `Foo`.  In Lisp,
in contrast, the insertion will proceed unimpeded, and the runtime
error message will point to the program fragment that expected the
`Foo`, implicitly blaming it for being unable to deal with the `Foo'`.
This leads to bugs that are difficult to track down, because the
actual mistake occurred long before and far away from the time and
place where it was detected.[^racket]

[^racket]: The one exception to this rule among Lisps that I know of
is the contract and blame system in Racket, which to my knowledge does
let one write generic container data structures that can be equipped
with use-site contracts restricting the things that can be added to
them, and laying blame at both the producer and the consumer of an
object if they disagree about the object's invariants.

For a while I thought the above was the whole story, but on further
reflection I see a deep advantage to the Lisp way, too.  The advantage
relates to programming in the large---composing compound systems from
parts developed by independent organizations, which themselves are
composed from parts developed by independent organizations.

Consider: you are writing some package that uses version 1 of some
library, and the library exposes a very useful function.  OK, fine, no
problem.  Package written, function called, everything works great.
Now suppose after a little while, the author of the library releases
version 2, which generalizes the original function by accepting a
broader range of inputs.  If this is happening in Lisp, your package
can link against version 2 _without modification_---the necessary
conversion from the constrained calling convention you are using to
the more general one the function now accepts happens implicitly, so
that your package will just work on a system that has version 2
installed.

On the other hand, in Haskell, the author probably had to change the
type of the input you need to pass to the library function.
So now you have to write that `foo_to_bar`
function and add a call to it in order to work with version 2 of the
library.  And doing so prevents you from working with version 1.
Oops.  This is a problem even if you wrote the package for your own
personal use, but it turns into a real pain point if there are more
users.  Suppose someone wants to use your package, and also some other
package that also uses the library under discussion.  In Lisp,
everything is fine: your package works with whatever version this
third user has.  In Haskell, either the package management system has
to deal with multiple versions of the library coexisting, and with
linking each package against the version it wants (which it doesn't,
though rumor has it that they are working to fix that),
or you and the author of the other package have to agree on which
version of the library you are supporting.  The latter is practically
impossible, because the obligation to agree was not even a choice
either of you made---it was imposed on you by a third party.  So it
doesn't happen, and the poor user cannot use your two packages
together.

The Haskell afficionado in the audience will doubtless point out many
ways the author of that library could have avoided having this
problem.  Yes, growth can be made smoother if one anticipates it.  The
point is that a strictly-safe style tends to increase coordination
burdens, which in turn tends to impede unanticipated growth, when
no one person controls the entire code base.

Of course, the situation I just described can happen in any
programming environment; and does---so often that it has a
[name](https://en.wikipedia.org/wiki/Dependency_hell).  A best-effort
style of trying to make things work out when possible reduces the
incidence of this problem, but doesn't actually solve it.  Does that
reduction change the quality of programmers' and library authors'
lives?  Do library ecosystems form and grow differently on best-effort
as opposed to strictly-safe platforms?

Notes
-----
