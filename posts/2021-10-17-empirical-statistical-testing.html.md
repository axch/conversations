---
title: Empirical Statistical Testing
date: 2021/10/17
author: Alexey Radul
published: false
---

I have been [thinking
about](/ideas/2016/on-testing-probabilistic-programs/) [statistical
testing](/ideas/2017/compositional-statistical-unit-testing/) for [a
while](/ideas/2018/statistical-testing-3/), but I've been bothered by
a disconnect between my previous theory and actual statistical
software practice.  To wit, I had started with tests of deterministic
software, and tried to back off as little as I could; but the result
proved too brittle.  My thoughts have since evolved, and, prompted by
a recent question on a work-internal mailing list, I want to write out
what I now think of as a more pragmatic approach.  READMORE

To recap, the problem with testing a random procedure, like, say, a
Gibbs sampler, or a stochastic expectation-maximization method, is
that even a correct program can occasionally produce weird results by
sheer bad luck.  Conversely, even a badly buggy program can
occasionally produce reasonable-looking results by sheer good (?)
luck.

What to do?  We're used to testing deterministic programs, where we
run our test once, and if the program produces the wrong answer, it's
definitely buggy, while if it produces the right answer, it definitely
doesn't have any bug that particular test can detect.

With random programs, the best we can do is to statistically separate
bugs from not-bugs.  A "good" test, then, is one that fails rarely,
say at most 1% of the time, when the program is correct; and passes
rarely, say also at most 1% of the time, when the program has the bug
that test is looking for.

How does one get a "good" test?  The basic technique is to run the
program under test many times, and check that its average behavior is
close to right; and then to tune "many" and "close".  Let's take a
reasonably typical case as an example.  Suppose you're trying to write
a function for computing confidence intervals on some task by
bootstrap.

but often the program under test is trying
to compute some (random) real number, and we know, at least
approximately, what that number should be on average.  Then we're
looking for bugs that move that average.
