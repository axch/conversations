---
title: How <em>to</em> Compute <em>with a</em> Probability Distribution
date: 2015/02/15
author: Alexey Radul
---

What makes a good representation for computing with probability
distributions?  The two canonical options are samplers and probability
density functions.  Both are valuable; and the relationship between
them turns out to hide two fruitful variations on the idea of a
sampler, READMORE that I will call "importanter" and "rejecter".
The purpose of this essay is to carefully study these four
objects and the interrelations between them, and the light they shed
on the ubiquitous rejection sampling and importance sampling algorithms.

The probabilistic programming system
[Venture](http://probcomp.csail.mit.edu/venture/) that I am working on
makes heavy use of the idea that a (computable) probability
distribution is very effectively represented by a stochastic machine
that computes samples from that distribution.  A couple months ago I
had the privilege of discussing this topic with [Ken
Shan](https://twitter.com/ccshan/), which conversation
caused me to think through these foundational relationships.  Any
elegance in the result is due entirely to Ken; the mistakes are of
course my own.

To make the content computationally concrete, I will spell out what I
am saying in pseudo-Haskell as well as English.  Why Haskell?  Because
its type system is rich enough to capture the interesting structure
very well.  In fact, don't read the code---read the type signatures.[^glossary]

[^glossary]: If you don't know Haskell, don't worry: I will say
everything in English first.  You will just have to take my word that
there are simple algorithms.  Here is a quick glossary for
how to interpret the type signatures, so you can follow the shape
of the argument:

    - `--` (two hyphens) begins a comment, which continues to the end
      of the line.

    - `::` (two colons) means "of type" or "has type".  It says that the
      expression on the left has the type given on the right.

    - `->` (rightward arrow) is an infix type operator meaning "function".
      That is, `Foo -> Bar` is a function taking an object of type `Foo`
      and returning one of type `Bar`.  The arrow associates to the right:
      `Foo -> Bar -> Baz` is `Foo -> (Bar -> Baz)`, which is a function
      taking a `Foo` and returning a function that takes a `Bar` and
      produces a `Baz`.  Such a beast is operationally equivalent to
      a binary function that takes a `Foo` and a `Bar` and produces
      a `Baz`, and Haskell automatically applies the equivalence whichever
      way is most convenient.

    - (whitespace) is type constructor application.  For example,
      one writes `Set Integer` to denote the type of sets of integers.
      So a function from sets of integers to sets of floating point
      numbers would have type `Set Integer -> Set Double`.

    - Haskell type signatures can have variables in them.
      `Set a -> Set a` means "a function from sets of anything to sets
      of the same thing."  Actually, it means something stronger than
      that: the function is taken to be [parametrically
      polymorphic](https://en.wikipedia.org/wiki/Parametric_polymorphism),
      which is to say it can't manipulate the individual objects of
      type `a`, but operate only on the set.

    - `[]` (square brackets) mean "list of" in Haskell.

    - `(,)` (comma-separated list in round brackets) is for tuples.
      The components of 2-tuples are accessed by the functions `fst`
      and `snd`.

    - In the code, (whitespace) is function application.  In Haskell,
      one writes `f x` to mean "apply f to x".  This choice is natural
      for a functional language, where one is applying functions all
      the time, but can be a bit confusing to someone who is not used
      to juxtaposition signifying that.  This operator has the highest
      precedence: `f x + 4` is "apply f to x, then add 4", not "apply
      f to x+4".  There is also the `$` operator, which is function
      application but with the lowest precedence, so `f $ x + 4` is
      "apply f to x+4".

Contents
--------

- [Sampling](#sampling)
    - [Expectation](#expectation)
    - [Measures](#measures)
    - [Operations on Samplers](#operations-on-samplers)
- [Densities](#densities)
    - [Composition of densities](#composition-of-densities)
- [Importance Weighting](#importance-weighting)
    - [Measuring Importance](#measuring-importance)
    - [Composition of Importanters](#composition-of-importanters)
    - [Weighted Expectations](#weighted-expectations)
    - [Importance Sampling](#importance-sampling)
    - [Proposals](#proposals)
    - [Resampling](#resampling)
- [Rejection](#rejection)
    - [Measuring Rejection](#measuring-rejection)
- [Relationship](#relationship)
- [Exchangeable Coupling](#exchangeable-coupling)
- [Notes](#notes)

Sampling
--------

<blockquote class="pullquote-display"><p>
A sampler is a machine that represents a probability distribution by its behavior.
</p></blockquote>

Let us start our exploration of representations of probability
distributions with the (exact) sampler.  A _sampler_ is a machine that
represents a probability distribution by its (random)
behavior:[^random-variables]

    type Sampler a  -- A source of random as.

[^random-variables]: Samplers are different from random variables as
traditionally defined.  One of the formulations of traditional random
variables is "(measurable) functions from a probability space".
Samplers are also functions from a probability space, namely the space
of unbounded numbers of uniform random bits.  The difference is that
traditional random variables can be functions from the same
probability space, and thus can exhibit dependence; whereas I treat
Samplers as getting independent random bits every time they are
called.  The two formulations are equi-expressive (at least if
restricted to computable situations): any system of random variables
can be modeled as a big Sampler for their joint distribution; and any
invocation of a Sampler can be viewed as a system of random variables
(one per intermediate value in the Sampler's computation).

I elide questions of the entropy source; think an infinite stream of
uniformly random bits in the sky.  Since I like computation, I will
also require the samplers I think about to terminate with probability
1.  A "good" sampler is one that delivers its samples quickly---with
little computation.

Samplers support the following three natural operations (which I will
name by their conventional names):

    return :: a -> Sampler a
    return x = "sample" by always emitting x, consuming no randomness

    fmap :: (a -> b) -> Sampler a -> Sampler b
    fmap f sx = sample a b by sampling an a and then calling f on it

    join :: Sampler (Sampler a) -> Sampler a
    join ssx = sample a (Sampler a) from the input,
               then sample an a from that

These operations make samplers composable.[^join]

[^join]: In case the `join` operation looks a bit strange, imagine
randomly choosing a coin and then flipping it once.  If the coins may
have different weights, that's a probability distribution (the choice)
over probability distributions (the possible biases of the flip).
`join` just tells us that we can view that compound process as a
single probability distribution over heads/tails outcomes.

Exercise: Prove that these definitions for `return`, `fmap`, and
`join` satisfy the [Monad
laws](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29#fmap_and_join)
and conclude that samplers form a monad.  If you don't know what I'm
talking about when I say "monad", don't worry---for the purposes of
this post, a sufficient intuition is that composing samplers is
"well-behaved".

Note that there is no performance penalty for composition: the cost of
sampling from the output is a direct consequence of the costs of
running the inputs, without significant overhead.


### Expectation

If we have a sampler over $\R$ (actually, any vector space, but $\R$
will do), we can form finite estimates of the expectation (which
estimates are themselves random) by computing some samples and
averaging.  In types and code that looks like

    finite_expectation :: Int -> Sampler R -> Sampler R
    finite_expectation n sx = fmap average (replicateM n sx)

Theorem ([Law of Large Numbers](https://en.wikipedia.org/wiki/Law_of_large_numbers)):
For any sampler `sx :: Sampler R`, as $n \to \infty$, the finite
expectations `finite_expectation n sx` converge (as distributions) to
`return mean_sx :: Sampler R` for some number `mean_sx`.

The constant `mean_sx` is of course the expected value of the
distribution given by the sampler `sx`.  The law of large numbers
gives us licence to call it *the* expected value of `sx`.  By abuse
of notation, we can write that idea down as (well-typed!) pseudo-code:

    expect :: Sampler R -> R
    expect sx = mean_sx where
      return mean_sx = infinite_limit (\n -> finite_expectation n sx)


### Measures

Once we have the idea of expectations, a sampler for any type `a` gives rise to a
[measure](https://en.wikipedia.org/wiki/Measure_%28mathematics%29) over
the set $A$ of all objects of type `a`.  This links our computational objects
to the standard mathematical foundations of probability theory.

Intuitively, the size of a subset under this measure is the
probability that the sampler produces an object from that subset.  Formally,
for `sx :: Sampler a`, and any subset $S \subset A$ given by an
indicator function $f_S : A \to \{0,1\}$, we can define $\mu(S)$ as
the expected value of composing the sampler with the indicator function:

$$ \mu(S) = \texttt{expect (fmap f sx)}. $$

Exercise: Prove that $\mu$ given by the above definition is a
[probability measure](https://en.wikipedia.org/wiki/Probability_measure).

Exercise: Prove that integration with respect to $\mu$ is expectation
under the sampler:

$$ \forall (c:A \to \R), \int c d\mu = \texttt{expect (fmap c sx)}. $$

Insofar as probability measures are accepted as a reasonable
definition of "what a probability distribution is", the two above
facts mean that a sampler can validly be said to represent
the probability distribution on its outputs.


### Operations _on_ Samplers

How well do samplers implement the operations we like to perform on
probability distributions?

-   Joint distributions: If the sampler `sx :: Sampler a` represents the
    distribution $p(a)$, and the function `f :: a -> Sampler b`
    represents the conditional distribution $p(b|a)$, then getting a
    sample from the joint distribution consists of drawing a sample from
    `sx`, applying `f` to get a sampler for `b`s, drawing a sample from
    that, and emitting the pair:

        joint :: Sampler a -> (a -> Sampler b) -> Sampler (a, b)
        joint sx f = do
          a <- sx
          b <- f a
          return (a, b)

-   Marginal distributions: If we have a sampler that represents the
    probability distribution $p(a,b)$, then drawing a sample from the
    marginal distribution $p(a)$ is just drawing a sample from the joint
    and throwing away the unneeded component:

        marginal :: Sampler (a, b) -> Sampler a
        marginal = fmap fst

-   Conditional distributions: However, a sampler for $p(a,b)$ does not
    easily lend itself to a sampler for the conditional $p(b|a=x)$.
    Conditional distributions are hard.

        conditional :: Sampler (a, b) -> a -> Sampler b
        conditional = ???

Exercise: Prove that the above two constructions actually work, namely
that `joint sx f` represents the correct joint probability measure and
`marginal sxy` represents the correct marginal probability measure.

The lack of natural samplers for conditioning is unfortunate, because
conditioning is an important operation.  Conditioning is arguably
*the* operation that gives probability theory its practical
significance, since it is the operation that poses the causal
inference problems we need probability for: "given a cause-and-effect
model $p(a,b)$, and given that effect $A$ happened, what causes $B$
for it are probable?"


Densities
---------

The other common representation for probability distributions is the
density function[^cumulative]. A _density function_ (or just density
for short) is a way to evaluate how "dense" a probability distribution
(measure) is at some particular value.  Such an evaluation is perforce
relative to some other measure, which is taken to represent our notion
of "uniformly dense" (even though it can really be pretty much any
measure on the same space).

    type Density a = a -> R  -- positive only; the base measure is implicit

[^cumulative]: One also talks about [cumulative distribution
functions](https://en.wikipedia.org/wiki/Cumulative_distribution_function) (CDFs)
when one talks about probability distributions over the real numbers,
but I won't bother because a CDF is just the integral of the density,
so carries (more of) the same sort of information.  Not all of the
subsequent discussion applies, because there is a good way to recover
a sampler from a CDF, but CDFs are also much less commonly available
than densities.

Given a base measure $\mu$ on `a`, a density `d :: Density a`
defines a measure $\mu_d$ by

$$ \mu_d(S \subset A) = \int_S d d\mu. $$

Exercise: Prove that the integration rule for $\mu_d$ is

$$ \forall (c:a \to \R), \quad \int c d\mu_d = \int d \cdot c d\mu, $$
where the multiplication on the right hand side is taken pointwise.
This is the expected value of the function $c$ under the probability
distribution given by $d$.

Corollary: If $\mu$ is a probability measure, then $\mu_d$ is also,
provided $\int_A d d\mu = 1$.

Exercise: Prove that $\mu$ and $\mu_d$ determine the density function
uniquely (up to the usual caveats of continuous analysis):

$$ d(a) = \lim_{\mu(S) \to 0} \frac{\mu_d(S)}{\mu(S)} \qquad \textrm{for } a \in S \subset A.  $$
Not all pairs $\mu$ and $\mu_d$ give rise to finite density functions
$d$, but exploring that topic would take us too far afield.

<blockquote class="pullquote-display"><p>
Both a density and a sampler give a probability distribution, but
they offer operationally different information about it.
</p></blockquote>

Both a `Density a` and a `Sampler a` give a measure on `a`, but they
offer operationally different information about it.  The sampler gives
a computational mechanism for drawing examples, the distribution of
which obeys the measure.  The density function gives a computational
mechanism for evaluating any given object under the measure.
Recovering either of these operations from the other requires
integration (either with respect to the base measure of the density or
the measure given by the sampler), which cannot in general be done
cheaply and exactly.

Consequently, it can be useful to carry both a sampler and a density
for the same measure:

    type Dist a = (Sampler a, Density a)  -- for the same measure
      -- the base measure of the density is implicit

Well-studied probability distributions typically have both efficient
samplers and efficient density functions, hence the name `Dist` for
the type.


### Composition _of_ Densities

Densities also technically obey the monad laws, but only if one is
willing to take integrals (or sums in the discrete case).  Since
integrals are awkward to express in code, I will record them in math.
Also, the integrals make more sense at the level of measures; the
actual densities can be derived as limits thereof as usual.

    return :: a -> Density a
    fmap :: (a -> b) -> Density a -> Density b
    join :: Density (Density a) -> Density a

$$ \begin{eqnarray*}
 \mu_{\texttt{return x}}(S \subset A) & = & \begin{cases} 1 \textrm{ if } \texttt{x} \in S \\ 0 \textrm{ otherwise} \end{cases}, \\
 \mu_{\texttt{fmap f d}}(S \subset B) & = & \int_{f^{-1}(S)} 1\ d\mu_{\texttt{d}}, \\
 \mu_{\texttt{join dd}}(S \subset A) & = & \int_{\textrm{densities } d \textrm{ on } A} \left( \int_S 1 d \mu_d\right) d \mu_{\textrm{dd}}.
\end{eqnarray*} $$

Exercise: Prove that the above operations are well-formed, that is
that if the arguments represent probability distributions then the
results do too.

Exercise: Prove that densities obey the monad laws with the above
operations.

How well do densities perform the operations we like on probability
distributions?

-   Joint distributions: If $p(a)$ is represented by a density, and $p(b|a)$
    is represented by a function from `a` to a density, then the joint
    distribution $p(a, b)$ is represented by the product:

        joint :: Density a -> (a -> Density b) -> Density (a, b)
        joint dx fxdy (x,y) = dx x * fxdy x y

-   Marginal distributions: Marginal distributions are actually the
    place where the integrals in the monad laws come from.  If
    $p(a,b)$ is represented by a density, then to compute the density
    of $p(a)$ it is necessary to integrate the density function over
    all possible $b$ (with respect to the projection of the base
    measure):

        marginal :: Density (a, b) -> Density a
        marginal dxy x = -- integral over y of dxy (x,y)

-   Conditional distributions: Conditional distributions are the place
    where densities show their true worth---taking conditional densities
    is just currying:

        conditional :: Density (a, b) -> a -> Density b
        conditional dxy x y = dxy (x,y)  -- unnormalized

There is actually an important subtlety in the last of these, which is
that the result `conditional dxy x` will not integrate to 1 over `y`
unless we scale it by the appropriate integral.  This is unfortunate,
because that integral is all too often intractable, but even an
unnormalized density is better than nothing.

Exercise: Prove that these formulas are correct, namely that the
results of `joint`, `marginal`, and `conditional` actually represent
the respective joint, marginal, and conditional measures (in the
latter case, up to multiplication by a constant).  In the case of
`joint`, the base measure on `b` should not depend on the value
passed to the function `fxdy`.

<blockquote class="pullquote-display"><p>
Much of the theory of Bayesian inference is a search for various
ways to turn a density into a sampler for the same distribution.
</p></blockquote>

Conditioning is why densities are interesting.  But samplers are nicer
to compute with.  Is there a way to get from a density to a sampler
for the same distribution?  Much of the theory of Bayesian inference
is a search for various ways to do that with acceptable performance
and acceptable degree of approximation.[^expect]  But let us start
with basics.

[^expect]: That is, sampling from a different distribution that is
easier to sample from but approximates the distribution given by the
density; or, in some cases, sampling from a different distribution
entirely, but whose samples in some way help to compute desired
expectations with respect to the density of interest.


Importance Weighting
--------------------

So, suppose we have a `Density a` and we want something like a sampler
for the same distribution.  What can we do?  Well, the density is
against a base measure, which we presumably understand.  So we can
perhaps draw samples from the base measure and weight them by the
density.  What would that get us?  Eventually it will get us to the
ubiquitous [importance
sampling](https://en.wikipedia.org/wiki/Importance_sampling) and
[rejection sampling](https://en.wikipedia.org/wiki/Rejection_sampling)
algorithms, but let's take it slow and think through each step as it
comes.

So, weighted samples:

    type Weight = R  -- should be non-negative
    type Importanter a = Sampler (a, Weight)

    weighted :: Sampler a -> Density a -> Importanter a
    weighted sx dx = do
      x <- sx
      return (x, dx x)

What is this object that we get as a result?  An importanter over `a`
is a machine that emits random values of type `a` together with the
weights (which are real numbers) those values should be given.
How meaningfully does an importanter represent a probability
distribution?


### Measuring Importance

We can capture the idea that the samples emitted by an importanter
"should be" taken with the grains of salt given by the weights, by
defining the measure on `a` that an importanter gives to take that
information into account.

Formally, consider `ix :: Importanter a`.  Being a sampler, `ix`
defines a measure $s$ on the set of pairs $A \times \R$.  For any
subset $S \subset A$ with indicator function $f_S$, we can define the
weight of $S$ under $s$ as

$$ W(S) = \int_{(x,w)} w f_S(x) ds, $$
which is the expected weight of elements of $S$ generated by `ix`.
Then we can define $\mu$ on $A$ as the fraction of the total expected
weight contained in $S$:

$$ \mu(S \subset A) = \frac{W(S)}{W(A)}, $$
provided the denominator is finite and positive.

Exercise: Prove that $\mu$ is a probability measure if $s$ is
and $0 < W(A) < \infty$.

Exercise: Prove that if `dx :: Density a` is a density and `sx ::
Sampler a` is a sampler for the base measure of `dx`, then `weighted
sx dx :: Importanter a` is an importanter representing the same
probability distribution as `dx`.

Exercise: Prove that the integration rule under $\mu$ is given by

$$ \forall (c:A \to \R), \quad \left(\int c d\mu\right) W(A)
  = \int \texttt{comp c } ds, $$
where

    comp :: (a -> R) -> (a, R) -> R
    comp c (x, weight) = (c x) * weight

This integration rule formalizes the idea that to make conclusions
about $\mu$ based on being able to compute with $s$, we have to weight
every `x` we get out of $s$ by the weight it came with, and discount
our overall conclusions by the overall weight $W(A)$.

### Composition _of_ Importanters

is enough like composition of samplers that I omit the discussion to
save space.

### Weighted Expectations

The integration rule for the measure denoted by an importanter
tells us how to compute expectations with weighted samples, which
conveniently agrees with what one would expect:

    finite_weighted_expectation :: Int -> Importanter R -> Sampler R
    finite_weighted_expectation n ix = fmap w_avg $ replicateM n ix where
      w_avg :: [(R, Weight)] -> R
      w_avg samples = (sum $ map times samples) / (sum $ map snd samples)
      times (x, w) = x * w

Theorem (Law of Large Numbers with weights): For any importanter `ix :: Importanter R`, as
$n \to \infty$, the finite weighted expectations
`finite_weighted_expectation n ix` converge (as distributions) to
`return mean_ix :: Sampler R` where the constant `mean_ix` is the
expected value of the distribution on $\R$ denoted by the importanter
`ix`.

This theorem justifies the definition

    weighted_expect :: Importanter R -> R
    weighted_expect ix = mean_ix where
      return mean_ix = infinite_limit (\n -> finite_weighted_expectation n ix)


### Importance Sampling

Getting a (terminating, exact) sampler out of an importanter, however,
is more complicated.  The trouble is
that no matter how many times we've run our importanter, it's possible
that the next run will produce a new value with a huge weight, and
throw off all our previous conclusions.  But let's take that one step
at a time.

<blockquote class="pullquote-display"><p>
No matter how many weighted samples one has drawn, the next one might
have such a huge weight that it throws off all previous conclusions.
</p></blockquote>

There is of course an obvious and computationally efficient way to get
some sampler for `a` out of an importanter over `a`---just drop the
weights:

    importance_approximation :: Importanter a -> Sampler a
    importance_approximation = fmap fst

The trouble is, of course, that the sampler we get does not sample
from the distribution the importanter denotes---unless the weights are
all equal.  And indeed, the closer the weights are to equal, the
better an approximation it is to just drop them.  It turns out
that if we define

    weight_distribution :: Importanter a -> Sampler Weight
    weight_distribution = fmap snd

then we get the

Theorem: The quality of the `importance_approximation` goes
as the quality of the approximation `return . expect` to the
`weight_distribution`.

So a "good" importanter is one that uses little computation to run and
produces weights concentrated around one value.


### Proposals

Above, we constructed an `Importanter a` out of a `Density a` by
drawing samples from the base measure and weighting them by the
density.  The trouble is that if the density is peaky, this will yield
an importanter with a wide spread of weights, which may not be very
efficient.  If we can (efficiently) account for some of that variation
by drawing those samples from some other probability distribution,
that is perhaps closer to the target, we may be able to get a better
importanter.

To wit, we can use any `Dist a` (whose density has the same base
measure as the target) as a _proposal distribution_ whose samples we
can weight to make an importanter.  The weight of a proposal is its
value under our goal density, divided by its value under the proposal
density.

    proposal_to_importanter :: Dist a -> Density a -> Importanter a
    proposal_to_importanter (propose, prop_density) target_density = do
      sample <- propose
      let d_target = target_density sample
          d_prop = prop_density sample
      return (sample, d_target / d_prop)

[^same-measure]: Of course, this only works if the base measure
of the proposal density and the target density are the same.

Theorem: Given any proposal distribution `xs` and a target density
`target`, the importanter `proposal_to_importanter xs target` denotes
the same measure on `a` as the `target`, provided:

- the two densities are with respect to the same base measure on `a`
  (but it doesn't matter what that base measure is!), and
- and their ratio at every `a` is finite (that is, `xs` has a positive
  density at any `a` with positive density under `target`).

The division of densities also amounts to changing the base measure of
the target density to be the measure denoted by the sampler of the
proposal distribution.

A "good" proposal distribution for a given target is one that leads to
a good importanter---ideally, the proposal (and the density ratio) are
efficient to evaluate, but at least as importantly we want the density
ratio to be concentrated around the mean, rather than varying widely.
For that, we want the proposal distribution to be close to the target,
and in particular not to under-cover any region too severely (because
that can lead to very large weights).  In the limit where the proposal
distribution is exactly the target distribution, the weights always
come out exactly 1.


### Resampling

Even if we can't find a good proposal distribution,
we can trade work for a more concentrated weight distribution.  There
is a universal trick called _resampling_ for trading computation for
improving the importance approximation of any importanter.  It consists of computing $n$
weighted samples, picking one with probability proportional to the
weights, and emitting it with the combined weight of all the samples you
drew.  In a sense, that one sample summarizes the information gained
from the $n$ runs of the importanter.  In code:

    finite_resample :: Int -> Importanter a -> Importanter a
    finite_resample n ix = do
      samples <- replicateM n ix
      result <- weighted_select samples
      return (result, sum $ map snd samples)
      where weighted_select :: [(a, Weight)] -> Sampler a
            -- picks an element from the given list with probability
            -- proportional to its weight

Exercise: Prove that for any $n > 0$ and any `ix :: Importanter a`,
the measure on `a` given by `finite_resample n ix` is the same as the
measure given by `ix` itself.

Exercise: Prove that for fixed `ix :: Importanter a`, as $n$
increases, the `weight_distribution` of `finite_resample n ix`
concentrates, thereby improving the `importance_approximation`.

Theorem: In the limit as $n$ tends to $\infty$, the distribution
denoted by the sampler `importance_approximation $ finite_resample n
ix` converges to the distribution denoted by the importanter `ix`.

Exercise: Implement the resampling idea without knowing $n$ in advance
and without consuming intermediate storage that is linear in $n$.

    rolling_resample :: Importanter a -> [Importanter a]
    -- the nth element of rolling_resample ix should be equivalent to
    -- finite_resample n ix

The trick is that weighted selection is associative.

Why might resampling be of use?  That is, why throw out the samples we
(presumably) spent so much computation on instead of providing all of
them?  Because actually, fewer samples will require less computation
downstream; and the resampling rule will tend to pick samples with
large weight, so the samples that are thrown away were less important
anyway.[^particles]

[^particles]: In fact, drawing multiple independent samples from the
same base set and continuing the computation with all of them is also
useful, and also called resampling.  One abstract view of the thing
called a "particle filter" is interleaving resampling steps between
a series of `bind`s in the `Importanter` monad.


Rejection
---------

Having studied importance, let us turn to another foundational method
of creating samplers for new probability distributions.
[Rejection
sampling](https://en.wikipedia.org/wiki/Rejection_sampling) is the
probabilist's name for "generate and test"---make up an object, and if it is
"good", keep it, otherwise try again.  The great advantage of
rejection sampling is how little information it requires to operate;
little enough that it can serve as a definition for the idea of
conditional probability.

We can package up the generation part and the test part in a single
intermediate object that we can reason about as a whole:

    type Rejecter a = Sampler (Maybe a)

The way to read this is that a `Rejecter` over `a` is a sampler that
can fail: it either successfully produces `Just` an `a`, or produces a
sentinel value called `Nothing` that indicates failure.  The
relationship between rejecters and samplers is that one can recover a
sampler by trying a rejecter repeatedly until it succeeds:[^lazy-replicate]

    rejection :: Rejecter a -> Sampler a
    rejection r = fmap head $ fmap catMaybe $ replicateM r

[^lazy-replicate]: The attentive reader may notice that I am not
passing a count to `replicateM` here.  I mean a combinator that cannot
be defined in Haskell in general, which emits a lazy stream of results
from a monadic action.  For samplers this is OK though, because
drawing randomness commutes in distribution:

        replicateM :: Sampler a -> Sampler [a]

Computing expectations from a rejecter consists of turning it into a
sampler and computing expectations.

### Measuring Rejection

The link to measure theory lets us directly define which
distribution over `a` a given `Rejecter a` represents, without having
to appeal to the `rejection` algorithm to be definitional (and
therefore without having to re-analyze its behavior whenever the idea
of rejection appears):

By virtue of being a sampler, a rejecter denotes a measure over
`Maybe a`.  We can associate a measure over `a` with it by
saying that the size of any subset $S$ is the size of $S$ viewed as a
subset of $A \cup \{\texttt{Nothing}\}$, scaled up by dividing it by
the probability of the rejecter accepting (provided that probability
is positive).

Formally, given a `xs :: Rejecter a`, let $s$ be the measure on `Maybe
a` defined by `xs`.  For any subset $S \subset A$, we can abuse
notation to define $\texttt{Just } S$ to be the set of objects of type
`Maybe a` that are `Just` some element of $S$.  Then we can set

$$ \mu(S) = \frac{s(\texttt{Just } S)}{s(\texttt{Just } A)}, $$
provided the denominator is positive.[^compute-p]

[^compute-p]: The denominator $s(\texttt{Just } A)$ in this formula is
of course just the probability that `xs` accepts, that is returns
`Just` something as opposed to `Nothing`.  We can compute it (to
arbitrarily good approximation) as the expectation of the indicator
function for $\texttt{Just } A$ over `xs`, but we don't need to in
practice because `rejection` builds that correction in for us.

Exercise: Prove that $\mu$ is a probability measure whenever the
acceptance probability $s(\texttt{Just } A)$ is positive.

Exercise: Prove that integration under $\mu$ is given by the
rule

$$ \forall (c:A \to \R), \quad \left(\int c d\mu\right) s(\texttt{Just } A)
  = \int \texttt{comp c } ds, $$
where

    comp :: (a -> R) -> Maybe a -> R
    comp c (Just x) = c x
    comp c Nothing  = 0

One interpretation of this rule is that to turn the measure $s$ on
`Maybe a` into the measure $\mu$ on `a`, we just pretend that `Maybe
a` was `a`, except we demand that all users of the `Maybe a` interpret
`Nothing` results as "no effect" (which is what zero does for
integration), and scale their conclusions by the inverse of the
probability of acceptance.  In a manner of speaking, we moved the
rejection into the continuation.

Exercise: Prove the soundness of the `rejection` algorithm.  To wit,
for any `xs :: Rejecter a` that terminates with probability 1 and accepts
with positive probability, prove that

- the sampler `rejection xs` terminates with probability 1, and

- the measure $\mu_1$ on `a` given by `rejection xs` is the same as
  the measure $\mu_2$ on `a` given directly by `xs` through the above
  definition.

Note: The expected number of times the `rejection` algorithm will invoke
the rejecter is the inverse of the probability of acceptance.  Thus, a
"good" rejecter for $\mu$ is one that uses little computation per
attempt, and accepts with reasonably high probability, so that
applying `rejection` to it produces a good sampler.


Relationship
------------

Now it is time to tie these concepts together.

<blockquote class="pullquote-display"><p>
If we have an upper bound on the weights, we can recover an exact
sampler by converting those weights into probability of acceptance.
</p></blockquote>

The best we could do with just an importanter is to resample it some
number of times and hope the resulting approximation to the
distribution that importanter represents is good enough.  With a
little more information, though, it is possible to turn an importanter
into a rejecter (and therefore a sampler) denoting exactly the same measure.
To wit, if we somehow (analytically?) know
an upper bound on the weights produced by some importanter, we can
turn it into a rejecter that produces samples from the same
distribution:

    type WeightBound = Double

    importanter_to_rejecter :: WeightBound -> Importanter a -> Rejecter a
    importanter_to_rejecter bound xs = do
      (sample, weight) <- xs
      u <- unit_random
      if u * bound < weight then
        return $ Just sample
      else
        return Nothing

The intuition for this algorithm is that it translates the weight that
should be attached to any given `a` that comes out of `xs` into the
probability that this particular `a` will be accepted by the test.  In
order to do that coherently, though, an upper bound on weights is
needed, to make sure that all the probabilities are scaled correctly.
If the bound not tight, the resulting rejecter will accept less often
than it might.

Theorem: If `bound` is larger than any weight `xs :: Importanter a`
can ever return, then `importanter_to_rejecter bound xs` denotes the
same measure on `a` as `xs` does.

Conjecture: If the integral of returnable weights that are above the
`bound` is small, then `importanter_to_rejecter bound xs` denotes a
measure close to the measure denoted by `xs`.

Thus a "good" importanter for which one also knows a tight upper bound
on the returned weights leads to a "good" rejecter.  The exactness
provided by rejection comes at a price---one needs to have an upper
bound, and the rejecter will perform worse if one's bound is overly
conservative.

Now we have all the pieces to understand the standard names from the
field.  Importance sampling is usually presented as the composition
of proposal weighting, resampling some number of times, and dropping
the weights:

    importance_sampling :: Int -> Dist a -> Density a -> Sampler a
    importance_sampling n prop target =
      importance_approximation $     -- the result is approximate
      finite_resample n $
      proposal_to_importanter prop target

Rejection sampling is usually presented as a different composition,
of proposal weighting, converting weights into probabilities of
acceptance, and looping until an acceptable sample is generated:

    rejection_sampling :: WeightBound -> Dist a -> Density a -> Sampler a
    rejection_sampling bound prop target =
      rejection $
      importanter_to_rejecter bound $
      proposal_to_importanter prop target

I find the decomposition into distinct `Sampler`s, `Rejecter`s, and
`Importanter`s more aesthetic---and tending toward greater parsimony
and generality of analysis.

Exchangeable Coupling
---------------------

And now for something completely different that this view sheds light
on.

The phenomenon of [exchangeable
sequences](https://en.wikipedia.org/wiki/Exchangeable_random_variables)
arises from there being two different ways to get a `Sampler [a]` from
(a desired length and) a `Sampler (Sampler a)`.  That is, if you have
a machine that makes random machines that make random objects, and you
want a random sequence of objects, you have options, and they are not
the same.

A distribution on length-$n$ lists `xs :: Sampler [a]` is said to be
_independent and identically distributed (IID) with distribution `x`_ if
`x :: Sampler a` and `xs = replicateM n x`.  That is, as the name says,
each object was generated independently from the others from the same known
distribution.

A distribution on length-$n$ lists is said to be _exchangeable_ if all
permutations of a given list are equiprobable under it.  All IID
distributions are exchangeable, but not vice versa.

So, if you have a `Sampler (Sampler a)` and you want independent `a`s,
you can make a new machine for each object, and use it once:

    independently :: Int -> Sampler (Sampler a) -> Sampler [a]
    independently n xss = replicateM n $ join xss

makes an IID sampler with element distribution `join xss` (in
probabilist-speak, the one-element distribution marginalizing out the
machines).

On the other hand, you could also make just one machine, and generate
all your samples from it:

    exchangeably :: Int -> Sampler (Sampler a) -> Sampler [a]
    exchangeably n xss = join $ fmap (replicateM n) xss
                    -- = xss >>= (replicateM n)

This sampler is not IID,[^why-not] but is still
exchangeable.[^no-join]

[^why-not]: The distribution over a single element is still `join
xss`, but now they are coupled through the common machine.  To reprise
the choice of coins example, a sequence of flips generated by choosing
one coin with unknown bias and flipping it repeatedly is not IID,
because seeing the beginning of the sequence gives information about
the end by learning (something about) the bias of the coin.

Theorem (de-Finetti): All exchangeable distributions can (in principle) be
represented as an application of `exchangeably` to some distribution
over distributions.

[^no-join]: If you wrote `exchangeably` without the `join`, it would
produce a sampler for samplers for IID sequences.  However, you
wouldn't know a priori which IID sequence you were going to get, so
predictions about the future behavior of any one of them would be
affected by observations of its past behavior, by inferring the
internal structure of the machine.

Typical probabilistic programming languages make the difference
between `independently`, `exchangeably`, and `fmap (replicateM n)` a
pain to think about, because they implicitly `join` everywhere, so one
has to write one's code carefully to get the effect one wants.

Thanks
------

to Tanya Khovanova, Alex Plotnick, and David Wadden for reading drafts
of this.

Notes
-----

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    Macros: {
      R: "{\\mathbb{R}}",
      eps: "\\varepsilon"
    },
    equationNumbers: { autoNumber: "AMS" },
    noErrors: { disabled: true },
  }
});
</script>
