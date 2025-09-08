---
title: Probabilistic Programming Habits
date: 2016/03/21
author: Alexey Radul
---

Programming [intentionally random
programs](/ideas/2016/on-intentionally-random-programs) presents its own special
software engineering considerations, in addition to the usual ones.  I
have been surprisingly slow to realize this, but two years in to
working on a [probabilistic programming
platform](http://probcomp.csail.mit.edu/venture) I can recommend some
specific habits around software engineering of intentionally random
programs. READMORE

**Reproducibility**.  Ironic as this may sound, my first suggestion for
how to write programs that are supposed to be random is to make them
deterministic.  That is, put all the program's randomness into the
initialization of the pseudo-random number generator (PRNG),
and don't draw any more entropy from external sources.[^non-determinism]  Why?
Because then it's easy to save that initial entropy and reuse it to
reproduce the program's behavior: Regenerate exactly the same plot you
saw last time, exactly reproduce the conditions that caused that rare
crash your automated test failed with, etc.

Corollary: Manage your PRNG objects so that independent sections of
the program consume independent streams of random numbers.  This is
starkest if the program is parallel, because then each thread
absolutely needs its own PRNG state, or else they will both slow down
due to contention and become unreproducible again due to reordering;
but the advice applies to any notionally separate serial operations as well.

However: Initialize any new PRNG states your program creates
internally from deterministic entropy sources, namely a PRNG available
at the point of creation.  If you are worried about statistical
anomalies from PRNG algorithms that were not designed to support
parallel streams of random numbers, use a cryptographically strong
algorithm---either for the whole program, or at least to create the
necessary PRNG seeds.

**Plotting**.  Intentionally random programs are supposed to produce
probability distributions on outputs, and will tend to have
probability distributions on intermediate quantities as well.  Your
eyeballs are _much_ more efficient at extracting information about
aggregate behavior of distributions from plots than from printouts of
samples (even if the samples are the only thing the downstream
consumer of your program can use).  Write any needed code for plotting
early and use it often.[^plots]

**Crash Testing**.  The intentionally random programs I have encountered
tend to have several adjustable outer loops of the same kinds: a knob
for precision (i.e., number of independent samples drawn to push
variance down via the law of large numbers), a knob for accuracy
(i.e., number of steps in a Markov chain method, or number of trials
in an importance-weighted method, determining closeness of
approximation to the exact answer), and, for "machine learning" types
of tasks, a knob for number of training data points.

The habit here is to make enough of those knobs externally adjustable
to be able to do very fast sanity check runs: draw three samples from
two steps of inference on five training points projected to two
dimensions and get all the way to the end result of the program.  The
goal is to be able to have a development cycle of (ideally) tenths of
seconds for flushing out coding mistakes that don't depend on the
probability distribution of the intermediate computations.  Have a version of your
automated test suite that does only such crash tests, for rapid
feedback on glaring errors (even in sections of the code that would
normally take a long time to reach in production-size runs).

Small, fast test cases are a good idea for all software development.
In this setting, they are not enough: such tests will not catch
mistakes that push the distribution on outputs enough to matter but
not enough to be noticeable with only two or three samples taken.  For
me, it was a lesson of experience that crash tests are worth having
anyway.

**Statistical Testing**.  The goal of intentionally random programs is
to produce outputs that obey desired probability distributions, so
testing that they are working requires evaluating and comparing
probability distributions.  For a distribution represented by a
program that purports to draw samples according to it, this task is
surprisingly non-trivial.  Fortunately, this situation is exactly the
domain of classical (frequentist) statistics: runs of the program with
distinct initial entropy are identical independent experiments, that
can be repeated ad nauseam.  Therefore, the null hypothesis that the
program does not have any mistakes can be tested with arbitrary
discrimination power (for the low price of consuming arbitrary
electrical power).  Such test cases still require thinking to set up,
however: to find appropriate invariants-in-distribution to test, to
find appropriate statistical measurements for them,[^multidimensional]
to test equalities that are only expected to hold approximately, and
to choose how to actually trade off computation for discrimination
power.

**Golden Output Testing**.  My experience of maintaining
intentionally random programs has been that one still spends plenty of
time working on refactorings that are expected to produce exactly no
effect on the random portion of the computation: exactly the same set
of pseudo-random numbers is still called for, they are to be combined
in exactly the same way, and should produce exactly the same answer.

For this scenario, I recommend making it easy to write test cases that
save the pre-refactoring program's exact output as produced by a short
but end-to-end run with a given seed entropy, and compare it
bit-for-bit with output produced by the post-refactoring program with
the same inputs and seed entropy.  This is not difficult to set up,
and fills an important niche: It will catch mistakes during such
refactorings more effectively than your crash tests, but use much less
computation than your statistical test suite.

A word of caution on this kind of "golden file" testing: You should
only be comparing differences in thusly saved outputs to find the
mistake that caused them.  Edits that are expected to change the
program's behavior at the level of which pseudo-random numbers are
generated should invalidate the affected golden files completely, and
be assessed by your crash tests, statistical tests, and functional
tests.

**Continuous Integration**.  Intentionally random programs provide an
opportunity for a new sense of "continous integration".  In
traditional software engineering, that phrase means having an
always-on computer somewhere that gets notifications from the version
control system and reruns the test suite on every commit (or as
frequently as practicable).  The purpose is to provide developers with
rapid feedback on the status of the build and test suite, without
requiring them to use their personal workstations to run it all the
time.

When developing intentionally random programs, the opportunity to gain
insight from computation is unlimited, even if the program is not
changing very rapidly: one can always run one's statistical tests with
more samples (and therefore higher discrimination power), or repeat
one's tests with different initial entropy.  I do not, however, yet
have tools or best practices to recommend on this point.  Besides just
implementation effort, the challenge seems to be to find a good
balance between invalidating runs performed against old versions of
the program and reusing computation across changes that did not affect
the component or behavior under test.

Notes
-----

[^non-determinism]: And also squash all the traditional sources of
non-determinism that also apply to programs that aren't supposed to be
random: order-sensitive traversals of hash tables, race conditions in
parallel programs, etc.

[^plots]: I am not yet satisfied with the available standard plot
types.  Eyeballing histograms is better than nothing, but I expect
that any given project will want [probability-probability
plots](https://en.wikipedia.org/wiki/P%E2%80%93P_plot) and
[quantile-quantile
plots](https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot) ([what's the
difference?](https://v8doc.sas.com/sashtml/qc/chap8/sect9.htm)) for
comparing similarity of 1-D distributions, as well as domain-specific
visualizations of uncertainty in structured objects.

[^multidimensional]:
My go-to tests now are [Pearson chi-square](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test)
or the [G-test](https://en.wikipedia.org/wiki/G-test) for small-domain
discrete distributions,
[Kolmogorov-Smirnov](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)
for one-dimensional continuous distributions, and ad-hoc reductions to
the above cases for others (binning, projection).
