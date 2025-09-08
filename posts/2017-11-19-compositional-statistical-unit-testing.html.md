---
title: Compositional statistical unit testing
date: 2017/11/19
author: Alexey Radul
---

How do you unit-test a sampler?  Run it a bunch and see whether the
histogram looks good---but there's always _some_ chance that your test
will fail randomly even though the sampler is good, or pass randomly
despite a bug.  And if you try to have more than one test, the chance
of random errors goes up.  How big of a chance?  How much worse does
it get?  What to do?  READMORE

I've [written](../../2016/on-testing-probabilistic-programs/) about
this problem before.  That was a starting point.  In this post,
I want to

- Work out a general formalism for reasoning
  about the error rates of tests of stochastic software;

- Work out the rules of composition for tests in this formalism; and

- Build up to a usable test, namely discrete equality in distribution
  (one variant where the expected distribution is known and one where
  it is itself avaiable only as a sampler).

This post is not so good for the math-averse---I need the crutch of
precise reasoning to make sure I don't miss anything important.

Contents
--------

- [Calibrated Tests](#calibrated-tests)

- [Calibrated Test Suites](#calibrated-test-suites)

    - [Amplifying Confidence](#amplifying-confidence)

    - [Composing Tests](#composing-tests)

    - [Negating Tests](#negating-tests)

- [Usable Tests](#usable-tests)

    - [A Building Block](#a-building-block)

    - [Two-Sample Equality in Distribution](#two-sample-equality-in-distribution)

    - [One-Sample Equality in Distribution](#one-sample-equality-in-distribution)

- [Conclusion](#conclusion)

- [Notes](#notes)

Calibrated tests
----------------

Suppose we are trying to statistically assess the behavior of some
(presumably stochastic) software $s$.
Let $\S$ be the space of possible true behaviors of $s$, the software under test.
Then a single _calibrated test_ $T$ is a 5-tuple: $(\tau, S^g, S^b,
\alpha, \beta)$ where

- $\tau: \S \to \bool$ is a stochastic test procedure returning
  $\text{True}$ or $\text{False}$ (which presumably runs the software
  under test repeatedly and measures its behavior somehow),
- $S^g \subset \S$ is the set of behaviors of $s$ that are "good",
- $S^b \subset \S$ is the set of behaviors of $s$ that are "bad", and
- $\alpha \geq 0$ and $\beta \geq 0$ are error tolerances, such that
- the false-fail rate is low: if $s \in S^g$, then $p(\tau(s) = \text{False}) \leq \alpha$, and
- the false-pass rate is low: if $s \in S^b$, then $p(\tau(s) = \text{True})  \leq \beta$.

For example, consider the situation from [my previous post on the
subject](../../2016/on-testing-probabilistic-programs/): $s$ performs
some unknown computation and "fails" with unknown probability $\pf$.
In this case, the number $\pf$ completely characterizes the behavior
of $s$ that we are interested in, so $\S = [0,1]$.  We wish to bound
$\pf$, so we set $S^g = [0, 0.1]$.  Suppose our test procedure $\tau$
is "Execute $s$ independently $n = 10$ times, and pass if $s$ 'fails'
at most 3 of them, inclusive."  Then it's easy enough to compute that
$\alpha \geq 0.0128$ will satisfy the false-fail rate equation.  There
are many choices of $S^b$ and $\beta$ that will satisfy the false-pass
rate equation for this choice of $\tau$, one of which is $S^b = [0.7,
1], \beta = 0.011$.  As one might imagine, increasing $n$ can let us
decrease $\alpha$ and $\beta$, and enlarge $S^b$.

Note that the above test only makes guarantees about the outcome of
$\tau$ if $\pf < 0.1$ or $\pf > 0.7$, but says nothing for the
intervening interval.  This is a general phenomenon, because
$p(\tau(s) = True)$ is continuous in the tested behavior $s$.
Ergo, if $\alpha + \beta < 1$, the sets $S^g$ and $S^b$ must be
separated.  If $S^g$ and $S^b$ are connected to each other in $\S$,
this implies incomplete coverage: $S^g \cup S^b \subsetneq \S$.

There are two symmetric ways to think about this gap.  One is to say
that we, in testing $s$, know what behavior $S^g$ is acceptable.  In
this case, everything else is a "bug" which we would ideally like to
distinguish from $S^g$, but, unless $S^g$ is both closed and open in $\S$, we can't.  So
we settle for defining $S^b$ to be the bugs that are "severe enough"
that we commit to finding them.  In so doing, we balance coverage (a
large $S^b$) against confidence (small $\alpha$ and $\beta$), and
against the computational cost of $\tau$.

Symmetrically, we could say that we know what behavior $S^b$ is
unacceptable, and define $S^g$ as the behaviors that are "obviously
good" enough to commit to pronouncing them correct.  On this view we
are balancing the test's robustness (large $S^g$) against confidence
and computation.

Calibrated test suites
----------------------

So much for an isolated calibrated test.  How can we compose these
things?  Like traditional (deterministic) unit tests, we can negate
test assertions, and connect multiple assertions with "and" (or "or").
We just need to exercise some care with the resulting good/bad sets
and error rates.  Unlike tests of deterministic software, it is also
meaningful to repeat a calibrated test, as a way of using computation
to gain more confidence.  Let's work these out in reverse order.

### Amplifying confidence

We can generically trade computation for confidence by
repeating a test independently more than once.  Indeed, if $T = (\tau, S^g,
S^b, \alpha, \beta)$ is any calibrated test, we can drive down
the false-pass rate by forming $T_\text{and}^k = (\tau^k_\text{and}, S^g, S^b, k\alpha,
\beta^k)$.  Here, $\tau^k_\text{and}$ is "Run
$\tau$ independently $k$ times and return the logical 'and' of all the
results."  The only way for $\tau^k_\text{and}$ to pass is if all $k$
runs of $\tau$ pass.  If this is to be a false pass, i.e., if $s$ is
actually in $S^b$, the probability of each pass of $\tau$ is bounded
by $\beta$, which bounds the false-pass rate of $\tau^k_\text{and}$ by
$\beta^k$, as desired.  However, the chance of a false-fail goes up:
for $\tau^k_\text{and}$ to falsely fail, it suffices for at least one
of the runs of $\tau$ to fail even though $s \in S^g$.  The
probability of this happening is bounded by
$$ \text{if } s \in S^g\quad p(\tau^k_\text{and}(s) = \text{False}) \leq 1 - (1 - \alpha)^k \leq k\alpha. $$
The former inequality is tight if $\alpha$ was a tight bound on the
false fail rate of $\tau$, but the latter is generally conservative.
However, it's simple, and if $k\alpha$ is, as is usually desired, much
less than $1$, then the quadratic and higher terms in $(1 - \alpha)^k$
are likely to be negligible.

Symmetrically, we can drive down the false-fail rate by forming
$T_\text{or}^k = (\tau^k_\text{or}, S^g, S^b, \alpha^k, k\beta)$,
where $\tau^k_\text{or}$ is "Run $\tau$ independently $k$ times and
return the logical 'or' of all the results."  Since the error rate we
are driving down falls exponentially while the other only climbs
linearly, we can amplify any non-trivial initial calibrated test $T$ into one
with arbitrarily small $\alpha$ and $\beta$.  (In fact, with a suitable
"$k$ of $m$ passes" scheme, any initial error
rates satisfying $\alpha + \beta < 1$ are enough.)

### Composing tests

The calibrated test formalism gets more interesting when we consider
combining different tests.  Suppose we have
$T_1 = (\tau_1, S^g_1, S^b_1, \alpha_1, \beta_1)$ and
$T_2 = (\tau_2, S^g_2, S^b_2, \alpha_2, \beta_2)$.  Then we can get a more
specific test that's still calibrated.  Let $\tau_1 \cdot \tau_2$ be the
testing procedure that runs $\tau_1$ and $\tau_2$ independently and returns
the logical "and" of the answers.  Then
$$ T_1 \cdot T_2 = (\tau_1 \cdot \tau_2, S^g_1 \cap S^g_2, S^b_1 \cup S^b_2, \alpha_1 + \alpha_2, \max(\beta_1, \beta_2)) $$
is a calibrated test.  Why?  If the software is good according to both
$T_1$ and $T_2$, the false fail rate is bounded by
$$ p((\tau_1 \cdot \tau_2)(s) = \text{False}) \leq 1 - (1 - \alpha_1)(1 - \alpha_2) \leq \alpha_1 + \alpha_2.$$

Conversely, if the software is bad according to either $T_1$ or $T_2$,
then it has to obey one or the other false-pass rate equation, so
$\max(\beta_1, \beta_2)$ bounds the overall false-pass rate.
Note, by the way, that if we stick to the tight bound $1 - (1 -
\alpha_1)(1 - \alpha_2)$ rather than the simple $\alpha_1 + \alpha_2$,
the $\cdot$ operation on tests becomes associative.

What have we gained?  By being a little careful with the confidences,
we can form compound tests that ensure all of a set of desirable
properties $S^g_i$ obtain together, or, equivalently, weed out any of
a set of severe bugs $S^b_i$.  In other words, we can build up complex
specifications of "correctness" by intersecting simpler specifications
$S^g_i$, while maintaining reasonable coverage through unioning the
corresponding unacceptable sets $S^b_i$.  This is the basis on which
developers of stochastic software $s$ can construct a test suite---if
they take care to tighten the $\alpha$ bounds of the tests from time
to time so that the $\alpha$ of the overall test suite stays within
acceptable limits.

For the record, the symmetric definition of $\tau_1 + \tau_2$ as the logical "or" of
$\tau_1$ and $\tau_2$ gives us
$$ T_1 + T_2 = (\tau_1 + \tau_2, S^g_1 \cup S^g_2, S^b_1 \cap S^b_2, \max(\alpha_1, \alpha_2), \beta_1 + \beta_2), $$
which is a way of building up compound unacceptable behaviors by
intersecting simpler ones.

### Negating tests

Finally, no notion of a test suite is complete without the idea of a
test that is known to fail, because of a known bug in the underlying
software that just hasn't been fixed yet.  The corresponding concept
here is test negation: given a calibrated test $T = (\tau, S^g, S^b,
\alpha, \beta)$, we can invert the sense of $\tau$ by running it once
and negating the answer.  The resulting $\tilde \tau$ gives the calibrated test
$$ \tilde T = (\tilde \tau, S^b, S^g, \beta, \alpha),$$
which makes the same distinction as $T$ with the same confidence,
but with the reverse sense.

This does not fully cover the role "expected failure" plays in
deterministic testing.  The latter has the property that if some test
fails for an unknown reason that one does not currently have time to
deal with, one can mark that test "expected failure", get the test
suite to pass that way, and then return to investigating the problem
later.  Negating a statistical test does not reliably obtain the same
effect, because the software $s$ could be in the dead zone $\S - (S^g
\cup S^b)$, where neither $T$ nor $\tilde T$ will pass reliably.
Sadly, there is nothing that can be done at the level of opaque
calibrated tests in this situation.  Perhaps a practical testing framework should
have an additional marking whose effect is "run this test (to check
that it completes without crashing) but ignore the answer" to
accommodate this situation.

Usable tests
------------

Let us now turn our attention to using this machinery to build the
simplest actually usable calibrated tests: discrete equality in
distribution.  We will get two variants: the general version, where
the "expected" distribution is itself given by a sampler, and a more
efficient version where the "expected" distribution is an explicit
list of objects and their probabilities.  The latter is also called a
"goodness of fit" test.

The student of statistics will recognize that both of these tasks have
well-known half-calibrated tests (the $\chi^2$ family is popular).  I
call the standard tests "half"-calibrated because, while their
false-fail rates are well-understood, I am not aware of any
characterization of their false-pass rates.  The obstacle, presumably,
is that such a characterization requires understanding the behavior of
the test statistic for any possible "bad" state of the software under
test.

### _A_ Building Block

Let us start with a two-sample inequality in probability test, as
this will let us build both of our promised compound tests.  To
wit, suppose our software $s$ under test provides two operations,
$s_1$ and $s_2$, each of which returns true or false.  We wish to
check that $p_1 = p(s_1 = \text{True}) \leq p_2 = p(s_2 = \text{True})$.

Formally, our software $s$ is characterized completely by the two
numbers $p_1$ and $p_2$, so $\S = [0,1] \times [0,1]$.  Our desired
"good" region is $S^g = \{p_1 \leq p_2\}$.  $S^b$ must be disconnected
from $S^g$, but it would be nice to have a parameter by which we can
let it approach arbitrarily close; so let's pick $S^b = \{ p_1 \geq
p_2 + \eps \}$ for some $\eps > 0$.

What can be do for a testing procedure $\tau$?  Not much, really.  The
simplest option[^dynamic] is to run $s_1$ some number $n_1$ times, run
$s_2$ $n_2$ times, and count the numbers $k_1$ and $k_2$ that they
respectively produce True.  Having obtained those results, use some
decision rule to emit True or False from the test as a function of
$k_1$ and $k_2$, and then calibrate $\alpha$ and $\beta$ by computing
or bounding the probability of False if $p_1 \leq p_2$, and of
True if $p_1 \geq p_2 + \eps$.

[^dynamic]: Not the only option, as one can imagine choosing $n_1$ and
$n_2$ dynamically, as results from previous trials come in.  Such a
technique may be a way to save computation, but a sound analysis is
beyond the scope of this blog post.

I don't have any special insight on the best way to choose such a
decision rule, but here's one that will do:
$$ \tau(n_1, n_2, \eps)(s) = \begin{cases} \text{True} & \text{if } \frac{k_1}{n_1} \leq \frac{k_2}{n_2} + \frac{\eps}{2} \\
  \text{False} & \text{otherwise}. \end{cases} $$

What false-fail rate does this have?  Well, if $s$ is actually
characterized by $p_1, p_2$, then the probability of any given outcome $k_1, k_2$ is
$$ p(k_1, k_2|p_1, p_2) = {n_1 \choose k_1} p_1^{k_1} (1 - p_1)^{n_1 - k_1}
  {n_2 \choose k_2} p_2^{k_2} (1 - p_2)^{n_2 - k_2}. $$
The probability of failure is the probability that any of the outcomes
leading to False will occur, and the worst-case false-fail rate
is that probability for the $p_1 \leq p_2$ that maximize it:
$$
\begin{align*}
 \alpha & = \max_{p_1 \leq p_2} \left[ \sum_{\frac{k_1}{n_1} > \frac{k_2}{n_2} + \frac{\eps}{2}}
  {n_1 \choose k_1} p_1^{k_1} (1 - p_1)^{n_1 - k_1}
  {n_2 \choose k_2} p_2^{k_2} (1 - p_2)^{n_2 - k_2} \right] \\
 & = \max_{p_1 \leq p_2}
  \left[ \sum_{k_2} {n_2 \choose k_2} p_2^{k_2} (1 - p_2)^{n_2 - k_2}
    \left( \sum_{\frac{k_1}{n_1} > \frac{k_2}{n_2} + \frac{\eps}{2}}
      {n_1 \choose k_1} p_1^{k_1} (1 - p_1)^{n_1 - k_1}
    \right) \right]. \\
\end{align*}
$$

The inner sum is convenient, being the survivor function of the
binomial distribution of $n_1$ trials with success rate $p_1$.  Since
the binomial survivor function is at every point monotonic in the
success probability, it's safe to say that for any given $p_2$, the
maximum over $p_1$ is at the highest value admissible by the
constraint, in this case $p_1 = p_2$.  This reduces our optimization
problem to one dimension.

The false-pass rate is similar:
$$ \beta = \max_{p_1 \geq p_2 + \eps}
  \left[ \sum_{k_2} {n_2 \choose k_2} p_2^{k_2} (1 - p_2)^{n_2 - k_2}
    \left( \sum_{\frac{k_1}{n_1} \leq \frac{k_2}{n_2} + \frac{\eps}{2}}
      {n_1 \choose k_1} p_1^{k_1} (1 - p_1)^{n_1 - k_1}
    \right) \right], $$
with a similar simplification due to the inner sum being a binomial
cumulative distribution function, which is maximized by minimizing
$p_1$.

The above optimizations are not very hard to solve.  Here are a few
computed error rates for tests with given $\eps$, $n_1$, and $n_2$
parameters:

$\eps$   $n_1$   $n_2$   $\alpha$   $\beta$
------   -----   -----   --------   --------
  0.20      30      30   0.183155   0.253557
  0.20     100     100   0.072367   0.082141
  0.20     200     200   0.020901   0.023023
  0.20     271     271   0.009036   0.009910
  0.10      30      30   0.349442   0.347353
  0.10     100     100   0.218377   0.260811
  0.10     300     300   0.102815   0.117143
  0.10    1000    1000   0.011947   0.013132
  0.10    1085    1085   0.009634   0.009974
  0.05      30      30   0.448711   0.397353
  0.05     100     100   0.361886   0.361332
  0.05     300     300   0.270163   0.269769
  0.05    1000    1000   0.127058   0.136325
  0.05    3000    3000   0.025816   0.026889
  0.05    4334    4334   0.009880   0.009995
------   -----   -----   --------   --------

### Two-sample equality _in_ distribution

With that capability in hand, we can now ask for something practically
usable that I don't know any other way to get: a calibrated test that
two discrete samplers are sampling from the same distribution.  To
wit, suppose $s$ now consists of two programs $s_1$ and $s_2$, each of
which randomly emits one of $D$ tokens.  We wish to test that $s_1$
and $s_2$ have the same probability of emitting each token.

This can be accomplished with a conjunction of $D$ of the two-sample
inequality tests described above: just assert that the probability of
$s_1$ generating each token $d$ is no more than the probability of
$s_2$ generating the same token:
$$ \text{And}_{d \in D} \big[p(s_1 = d) \leq p(s_2 = d)\big]. $$
The reverse inequalities follow by conservation of belief---the
total probability in both distributions must be 1.

One could use this to, for example, design a calibrated unit test for,
say, a Chinese Restaurant Process sampler.  The CRP is a probability
distribution on partitions.  There are 18 possible partitions of a set
of 4 distinct objects into clusters.  So, one could design a unit test
that checks whether two purported samplers for the CRP distribution on 4 objects in
fact agree.  In this style, that test would check, independently for
each of the 18 possible partitions, that the probability of drawing
that one from program 1 is no more than the probability of drawing it
from program 2.  If one designed each test to make $n$ trials, one
would need $18n$ trials in total, and, using the above
single-inequality procedure, one could obtain the following
computation-precision-confidence trade off:

$\eps$   $n$        $18n$     $\alpha$   $\beta$
------   --------   -------   --------   --------
  0.20         30       540   0.973787   0.253557
  0.20        100      1800   0.741313   0.082141
  0.20        300      5400   0.109433   0.007154
  0.20        530      9540   0.009010   0.000531
  0.10         30       540   0.999564   0.347353
  0.10        100      1800   0.988144   0.260811
  0.10        300      5400   0.858134   0.117143
  0.10       1000     18000   0.194546   0.013132
  0.10       2120     38160   0.009576   0.000572
  0.05         30       540   0.999978   0.397353
  0.05        100      1800   0.999692   0.361332
  0.05        300      5400   0.996548   0.269769
  0.05       1000     18000   0.913356   0.136325
  0.05       3000     54000   0.375490   0.026889
  0.05       8480    152640   0.009889   0.000573

If those trial runs look a bit large, there are couple reasons:

- Each individual test of the 18 is pessimistic, guaranteeing its
  error rates even if the probability of that particular partition is
  the most confusing possible, namely close to 1/2.  But, of course,
  they can't all be close to 1/2.  A directly synthetic test like the
  $\chi^2$ test of independence is likely to be much more efficient.
  Learning how to calibrate its false pass rate would be a useful
  advance.

- Relatedly, the design outlined here wastes computation on assuring
  the independence of the individual tests.  Namely, when one is
  measuring the frequency of some partition, one could presumably use
  those samples to measure the frequency of each of the 17 others as
  well.  I think it would be fruitful to think through what
  restrictions, if any, there are on sharing samples across tests of
  different facets of the same sampler.

- In this particular design, I arbitrarily chose that $\tau(n_1, n_2, \eps)
  = \text{True}$ when $k_1/n_1 \leq k_2/n_2 + \eps/2$.  That threshold
  balances the false fail and false pass rates for each individual
  test, but the compound test, being an "and", has a substantially
  higher false fail rate than each component, but the same false pass
  rate.  Therefore, some more efficiency can be obtained by making the
  threshold larger than $k_2/n_2 + \eps/2$.  That way, the computation
  used to ensure the false fail rate is acceptable is not wasted on
  driving the false pass rate lower than needed.

### One-sample Equality _in_ Distribution

We can actually gain a lot of computational efficiency if we know the
expected distribution analytically.  Then we can use a one-sample test
on each dimension, and save many trials by not having to account for
too many hard-to-assess probabilities that are near 0.5.

In the case of the Chinese Restaurant Process, the analytic
distribution on partitions is known.  For example, if the
concentration parameter is 0.5, the 18 partitions have these
probabilities:

Partition    Probability
------------ ----------------
[1, 1, 1, 1] 0.457142857143
[1, 1, 1, 2] 0.0761904761905
[1, 1, 2, 1] 0.0761904761905
[1, 1, 2, 2] 0.0380952380952
[1, 1, 2, 3] 0.0190476190476
[1, 2, 1, 1] 0.0761904761905
[1, 2, 1, 2] 0.0380952380952
[1, 2, 1, 3] 0.0190476190476
[1, 2, 2, 1] 0.0380952380952
[1, 2, 2, 2] 0.0761904761905
[1, 2, 2, 3] 0.0190476190476
[1, 2, 3, 1] 0.0190476190476
[1, 2, 3, 2] 0.0190476190476
[1, 2, 3, 3] 0.0190476190476
[1, 2, 3, 4] 0.00952380952381

Using the one-sample test design function from [my previous
post](../../2016/on-testing-probabilistic-programs/), we can design 18
tests corresponding to these probabilities.  These designs will
naturally spend less computation on the low-probability partitions,
because those are easier to reliably falsify.  With that design, we
can get to $0.01$ error rates with these total trial counts:

$\eps$   Trials
------   ------
0.2      1244
0.1      3894
0.05     12813

A dramatic improvement over the two-sample situation.

Conclusion
----------

I think the composable structure of calibrated tests forms a
sufficient backbone for an intellectually sound unit testing framework
for stochastic software.  Of course, a few more things do need to be
worked out before one can become practical.  Some come to mind
offhand:

1. It would be nice to get more efficient calibrated tests (perhaps in
   the $\chi^2$ style) for equality in distribution than those
   presented here.  I just started with these to prove that it could
   be done, and to demonstrate how the compositions tend to work out.

2. Continuous distributions can be attacked by binning, but it would
   be more satisfying to calibrate something like the
   [Kolmogorov-Smirnov](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)
   test.

3. Any library would do well to provide tests of nearness (as opposed
   to equality) in distribution, with the distance bound given either
   as an explicit number or implicitly as the distance between two
   other distributions.  This should be particularly useful for
   testing approximate inference algorithms, which promise only to
   move a distribution toward a goal, not to actually get there.
   Designing such tests directly could be substantially more efficient
   than composing them out of 1-D probability inequalities.

4. There is an interesting design space of continuous integration
   tools, since now there is confidence to be gained by rerunning a
   test that already passed, or increasing its computation budget
   internally.

5. I've presented forward computations, deriving the best obtainable
   error bounds from a given test procedure.  I think a tool could
   become substantially more practical if it automated the experiment
   design problem: The user tells the framework the $S^g, S^b,
   \alpha$, and $\beta$ they want, and the framework finds parameters
   for the testing procedure(s) $\tau$ to meet those requirements
   while using as little computation as it can.  This becomes very
   interesting when compound tests are involved, because the framework
   could (within limits) squeeze more confidence out of cheaper
   individual tests, and go a bit easier on expensive ones, and still
   get its overall error rate bounds.

But, that's all!

Notes
-----

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    Macros: {
      S: "{\\mathbb{S}}",
      bool: "{\\mathbb{B}}",
      pf: "p_{\\text{fail}}",
      and: "\\text{and}",
      eps: "\\varepsilon"
    }
  }
});
</script>
