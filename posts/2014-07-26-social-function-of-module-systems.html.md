---
title: Social Function <em>of</em> Module Systems
date: 2014/07/26
author: Alexey Radul
---

Observing the state of modern software practice, one might notice that
a rough tower of different abstraction mechanisms has emerged.  One
might then wonder: why do we need so many concepts, if their
fundamental job is to give a name to some software, and allow one to
use it just by referring to its name? READMORE

Indeed, why do we need

- functions (procedures, methods), _and_
- classes (in languages that have them), _and_
- modules, _and_
- packages, _and_
- distributions (such as the Debian or Red Hat distributions of GNU/Linux)?

What drives the choice of one or another mechanism when deciding how
to abstract something?  What drives a language designer's choice of
how to make one or another of these kinds of mechanisms behave?

I hypothesize that the answers are at the bottom social.  Let us
examine these different abstraction mechanisms in turn.

Functions
---------

The size of functions is bounded from below by them being the smallest
unit of code resue (or code sharing, or abstraction); and from above
by huge code style pressure to keep them small.  (Which I agree with!)
Partly by being the smallest and most fundamental, functions are more
or less required to be recursive (that is, a function can call another
function, and in functional languages can also define, emit, or
consume another function) and configurable (functions generally take
arguments).

Classes
-------

The size of classes is bounded from below by them (often) being the
smallest first-class[^first-class] unit of code reuse (sharing;
abstraction).  Because of this, there is pressure on classes to be
parametrizable and recursive in the same way as functions---hence
constructors taking arguments, the ability to take instances as
arguments to methods, and Java's inner classes, for example.

[^first-class]: By "first-class" I mean that historically,
object-oriented languages have tended not to allow class methods to be
stored in variables or data structures, passed as arguments, and so
forth---that privilege is reserved for instances of classes.

In languages with classes, they (especially "toplevel" classes) also
seem to serve the same function as the smallest modules do in
functional languages, which puts pressure on classes to have the
properties of modules also---hence the idea of "private members" that
cannot be read or written from outside the class's own methods.

Modules
-------

The word "module" actually means many different things in different
programming languages.  Here I will talk about what a "module" is in
Python, Haskell, or Racket.  An overlapping function is served by
"namespaces" in Clojure and C++ and "packages" in Java; though in the
latter two, a (large) class often serves this function as well.

Modules, at least the smallest ones, tend to correspond to chunks of
code that one programmer can be expected to keep in their head at
once, after some study.  Hence the tendency for one module to occupy
exactly one source file.  Also, boundaries between developers on a
team tend to fall along module boundaries.[^multiple] This is why the
question of "information hiding" arises: the user of a module is
pretty likely to be a different person from its author/maintainer, so
reasonable effort should be made to keep them from having to
synchronize changes (in either direction!)

[^multiple]: Typically, one programmer on a team will understand
several modules (and their interactions).  I would suggest that good
style is to make modules as small as possible such that any module
boundary can be a person-understanding boundary.  That gives the
greatest flexibility in adjusting which people carefully understand
what.

This tendency changes the demands on modules as an abstraction
mechanism.  If one wants subtle recursion or parametrizability, the
thinking goes, one can use a function or a class.[^ml] What is needed
from a module, instead, is the ability to use it easily, without
having to understand its internals, and without having to react to
someone else changing its internals.  In compiled languages, this
often comes with the desideratum of separate compilation: rebuilding
the full system after an internal change to one module should ideally
take time proportional to the size of that module, plus perhaps the
number of modules, but not the total size of the whole system.

[^ml]: The module system of Standard ML is one major exception, but
I do not understand it enough to comment.

Packages
--------

Packages are to organizations as modules are to individuals.  The
size and scope of a package is bounded above by the social requirement
to be able to enforce at least some policies across the whole package.
For example, typical package systems rely on module names being unique
within a package.  Hierarchy appears, either in the module system or
in the package system of a programming language, because organizations
are often hierarchical.  That way, at least for names, each level in
the hierarchy need only enforce relatively local uniqueness, but fully
qualified pathnames end up globally unique.

Packages are also the smallest unit of software distribution---that
is, of the transfer of software from one organization to (members of)
another.  That imposes a heavy requirement that the insides of
packages leak very little across their published interfaces; but it
also makes configurability and recursion even less of an issue than
for modules, because configurability must be preceded by understanding
to be useful.

Distributions
-------------

One more phenomenon occurs these days, which is "distributions".  The
paradigmatic instance of distributions is GNU/Linux, though many
programming languages besides C effectively have distributions of
their software as well: Hackage for Haskell, Quicklisp for Common
Lisp, C{P,T,R}AN for Perl, TeX and R, Rubygems for Ruby, etc.

I think of a
(good) distribution as sort of a co-package: organization-level
consistency, but by and for consumers rather than producers of
packages.  The thing that a distribution is supposed to accomplish is
defining a coherent universe of possible packages (including with
unique package names), such that any subset of them (that obeys the
dependency relation) can coexist and be simultaneously usable.  Of
course, there are a variety of global resources that packages may
compete for (name space of program elements, name space in the file
system for e.g. temporary storage, exclusive control of various
devices, etc), so this is in general a very difficult and ongoing
problem.

A distribution is the level at which the [diamond dependency problem](https://en.wikipedia.org/wiki/Dependency_hell)
becomes severe.  If there are, in fact, global resources that packages
compete over, then there may be situations where two packages cannot
coexist in the same system.  Most often this happens when the "two"
packages are actually different, slightly incompatible versions of the
same package.  They will naturally tend to define the same names, but
differently, and try to use the same resources.

Anyway, if it is possible for two packages to be unable to coexist,
then (in current package management systems) that relation tends to be
contagious: if A depends on B and C depends on D where B and D cannot
coexist, then typically A and C also cannot coexist.  And this is a
screw: even if no one in their right mind would want B and D together
(e.g., because D is actually B-2.0), someone in their right mind could
very well want A and C together, because they could be completely
unrelated.  And if A is produced by one organization, C by another,
and the "someone" resides in yet a third, that someone has a real
problem.

Such situations tend not to be big problems at smaller levels than
packages, because then A, B, C, and D are typically all in the same
organization, so there are channels for communication and leverage to
resolve the problem (typically either by making B and D compatible, or
by changing either A or C so they depend on the same thing such that B
and D need not coexist).  But at the level of packages, where
production and use are supposed to cross organizational boundaries,
diamond dependencies can be a severe headache.  I know I have been
bitten by them several times.

The outstanding challenge for the design of a programming language's
module or package system is therefore this: Make it possible to write
packages in such a way that two distinct but similar versions of the
same package can coexist.  Maybe not "be usable together directly from
the same third package", but at least "be usable together through a
diamond".  In other words, what a package depends on should not leak
through to affecting what it can interoperate with.

Notes
-----
