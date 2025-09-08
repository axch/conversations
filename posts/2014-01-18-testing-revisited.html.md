---
title: Testing Revisited
author: Alexey Radul
date: 2014/01/18 10:10:00
---

Test Driven Development, as it is now called, is something I have
[advocated](http://web.mit.edu/~axch/www/programming_habits.html)
for a large part of my career.  My more recent experience, however,
has brought more nuance to my view of the proper level and kind of
software testing, READMORE which now feels ripe for setting down.

The driving observation is that automated tests bear a cost.  Tests
are code, and like all code they require maintenance.  In the case of
tests, this cost comes in the form of making it harder to change the
interfaces against which the tests are written.  When such a change is
made intentionally, the tests need to be updated; and too many times
I have seen situations were doing so was even more difficult than
updating the production code, because it was not clear what, exactly,
the test was actually testing.

Don't get me wrong---testing software is by and large done too little
rather than too much.  If the interfaces you test against are
sufficiently obvious that you know you got them right, then the cost
of test maintenance will be minimal.  And aggressive testing improves
the shape of the underlying program, because it forces there to _be_
interfaces around parts that are sufficiently complex to be tested in
isolation.

That said, I think it is too easy for a carelessly cultivated test
suite to grow weeds---too many tests that each test too little.  In
particular, if a project is only tested with examples (to wit, in
exact situation X, the system should do this, that and the other), the
developers can easily lose track of what exactly each of those
detailed situations were supposed to stress; and when an interface
changes, get caught in a bind between fearing to throw a potentially
valuable test out, and not knowing how to adapt it to the new order.

I find that property-based testing goes a long way to alleviate this
potential problem.  The idea with
property-based testing is that you specify, as code, properties that
the system under test should satisfy under all (or a wide range of)
circumstances, and the test framework takes it upon itself to generate
circumstances, check the properties, and let you know if any of them
ever failed.  Typically, "circumstances" are different inputs to some
function under test, and are typically generated randomly; Haskell's
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and its [many ports](https://en.wikipedia.org/wiki/QuickCheck) are the prototypical examples of this
style.  Randomized property testing covers many testing situations,
but in principle it is also possible to use property tests with model
checkers (that systematically test the system against all "small"
inputs, a la [SmallCheck](https://hackage.haskell.org/package/smallcheck)), or white-box input generators (that
instrument the tested code and try to poke it in ways that elicit
different code paths), or even theorem provers (that prove that (a
model of) the system cannot violate the stated property).

For example, if one is developing a web application, one might wish to
ensure that users interacting with it do not cause the server to
crash.  One way to do so is to monitor the live server, and every time
it crashes, investigate what happened, and encode that situation in a
regression test that does that thing and checks that the server didn't
crash.  Then the server will stop crashing for that reason as long as
tests are kept passing.  This is a fine strategy, but it can be
greatly augmented by adding a randomized test that simulates random
user actions (including putting random data into web forms in the
application, etc) and reports any such sequences that cause the
server to crash.

Property-based
tests strike me as much easier to maintain than large collections of
example-based tests.  First off, one property test usually does the
work of very many example tests, because the actual examples are
generated automatically.  Second, even more important, the content of
a property test is clearer, because it is separated from the
specification of the circumstance in which that content is being
checked.

On the other hand, property-based testing alone is also unlikely to
fully serve a project's needs, because "do the right thing" can be
very hard to capture as an executable specification.  So some examples
are invaluable, because they can check that everything went right in a
particular situation, even things that are difficult to pin down as
being right until they go wrong.  Also, I expect it to be quite rare in
practice for a random process to generate really thorough coverage of
all circumstances that will occur in real use; so when something bad
happens to the live system, encoding that particular situation as a
regression test can be a good idea.

So how should one combine these styles?  Judgement is of course called
for.  If you have a user story that you want to make sure works,
that's probably an example-based test.  If you are chasing some bug
and the investigation makes you go "X shouldn't have happened", then
perhaps "X never happens" is a good property-based test.  If you find
yourself writing a second test whose purpose is to check the same
thing as an existing test checks, but under different circumstances,
then maybe those should be replaced with a property.  And, of course,
the line between the two can be pretty blurry: your user story can
become a suite of property tests where you check that any reordering
and variation of some collection of user actions all achieve some
aspect of the goals in that user story (and do not crash the
application).

As generic starting point, though, I suggest really taking seriously
the idea of a test suite as executable documentation:

1) Good documentation should contain enough usage examples to teach
   how the software is to be used.  The test suite should contain
   those same examples as unit tests, checking that they do what the
   documentation says they do.
2) Good documentation should also describe general properties of the
   system, such as the space of valid inputs.  These should turn into
   property-based tests.

Ideally, between the above two types of tests, the project will
achieve 100% documentation coverage---every assertion made in the
documentation accurately reflected in the testing process.

3) Finally, if the system lives long enough to experience nontrivial
   bugs, it makes sense to have a collection of regression tests,
   where each test is a reproduction of a circumstance that led to a
   bug and an assertion of the correct behavior in that circumstance
   (which could be implemented in terms of checking properties).

It seems reasonable to me to keep these three kinds of tests separate
from each other.  Doing so would be a way to approach the real goal,
which is that looking at any given test should make it obvious what
that test was trying to test (for example, which sentence in the
documentation is being verified by a given test).  That way, tests
become easy to adapt to intentional changes in the system they are
testing.

In particular, I think that as the underlying software evolves, the
regression suite should be treated lightly (in the [Go player's sense
of that word](https://senseis.xmp.net/?Light)): if an intentional interface change breaks a regression
test against that interface, then it's quite possible that the old bug
is nonsensical now, in which case the regression test should just be
thrown out.  The other tests, in contrast, should almost certainly be
updated in response to changes to the interfaces they are testing.
