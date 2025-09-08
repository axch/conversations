---
title: What is a Sampler?
date: 2016/07/03
author: Alexey Radul
published: false
---

One answer is "A sampler is a representation of a probability
distribution, of course".  But why do we need such funny
representations as samplers for these probability distribution things?
What computational properties do samplers, as representations of
distributions, have?  What other kinds of objects have those, or
similar, properties?

This essay is an attempt to explore and lay out some of these
properties, and some representations that have them.  It is something
of a follow-on to my previous post [on computational properties of
samplers and
densities](/ideas/2015/how-to-compute-with-a-probability-distribution/).
I will coordinate in the sense of using the same style, but otherwise
make this post self-contained.

Synopsis
--------

Not to keep those in the know in suspense, the computational
properties I have in mind are:

- Estimating arbitrary integrals with respect to the represented
  distribution,

- With an accuracy guarantee (from the Central Limit Theorem),

- In embarrassingly parallel.

- Also, serving as an abstraction boundary, that is, allowing useful
  computation to proceed without knowing what the integral(s) of
  interest is(are) to be.

I will discuss four objects that have these properties, two standard
and two new (at least to me):

- A _sampler_ is a computation that draws samples from precisely the
  desired distribution.  A sampler necessarily represents a
  probability measure, i.e. is always interpreted normalized.

- A _weighted sampler_ is a computation that draws samples from the
  wrong distribution but fixes it with weights (also called an
  importance sampler).  A weighted sampler may represent a general
  (un-normalized) measure if the expected weight is not 1.

- A _harmonically weighted sampler_ is also a computation that draws
  samples from the wrong distribution and fixes it with weights, but
  the weights are meant to be combined with the harmonic mean instead
  of the standard one.  These arise, for example, when viewing a
  rejection sampler as a representation for the subprobability measure
  whose size is the acceptance probability.

- A _biweighted sampler_ is the most specific common generalization of
  the previous two kinds of samplers.


Probability Distributions
-------------------------

What is a probability distribution?  One standard intuition for the
idea is that it's a machine that can be invoked repeatedly to produce
random objects, with the "more probable" objects coming up more often.
The formal mathematical definition is different, but more awkward to
work with computationally, so I will take "randomized machine" as the
definition for now, and return to math later.  Let me write `Random a`
for the type of probability distributions over objects of type `a`.
Such a machine is also often called a _sampler_, when the speaker
wishes to emphasize its computational nature.

One way to interpret the randomness is as a relaxation of the standard
of solution to a problem.  If you ask me a question that calls for an
answer of type `a`, a probability distribution on `a` is a way of
saying "I don't know, but here is a box of guesses that will be right
on average."[^spread]
If `a` is a vector space, an object of type `Random a` has a
well-defined average directly.  Otherwise, one considers a query function
`f :: a -> Double`, and
discusses the _expectation of_ `f` _under_ `prob :: Random a`, which is
the average `Double` that one gets by drawing an `a` from `prob` and
applying `f` to it.

[^spread]: And whose spread indicates how wildly ignorant I am.

What use is being right merely "on average"?  A priori, not much, but
the [Law of Large
Numbers](https://en.wikipedia.org/wiki/Law_of_large_numbers) makes it
possible, in a great many cases, to trade computation for
confidence---if you want a better answer, just ask my box for a bunch
of guesses and, you guessed it, average them.  How big a bunch gives
how good an answer?  Good question.  We will return to it.

[^measure-theory]: In fact, averages are so central that they form
the standard measure-theoretic definition of a probability
distribution: a thing that knows how to compute expectations with
respect to itself.  That definition is mathematically more convenient
than the "random machine" definition I gave because it smoothly covers
distributions over continuous spaces, and also the primitive source of
randomness that `Random a` sweeps under the rug.  I did not want to
start with it, however, because it is not computationally effective.

This business of averages is so mathematically (but, as we will see,
not computationally) well-behaved, that it actually is the standard
definition: a _measure_ on `a` is a thing that knows how to compute
expectations with respect to itself.   Let's write that down:
The only thing a measure can do is compute expectations, so might
as well represent it by that function.[^reals]

```
newtype Measure1 a = Measure1 { expect :: (a -> Double) -> Double }
```

[^reals]: The only thing we need from the real numbers in this
definition is that they be a vector space over a field of
characteristic zero, but I the class constraints and type variables
needed to seize that generalization would clutter the rest of the
post for too little gain.

One might wish, on the basis of this definition, to write a function
`measure` that computes the measure
represented by a given sampler.  But, what if the sampler ranges over
an infinite (or even very large) number of different `a`s?  How can we
hope to precisely determine the average `f a` in a finite
(respectively, practical) amount of time?

We can repeat the trick from four paragraphs ago of falling back to a
random answer which is right on average.  In this case, we can convert
a sampler into a random measure, such that the average of all those
measures is the one the sampler represented.

```
measure1 :: Random a -> Random (Measure1 a)
measure1 sampler = do
  a <- sampler
  return (Measure1 \f -> f a)
```

The measures in question are all trivial single-point measures, but
measures themselves can be averaged, so we can use the Law of Large
Numbers to approach the truth as closely as we wish.

TODO: This notion of "represent" should commute with join.
- A `Random (Measure1 a)` represents a `Measure1 a` by infinite averaging.
- So a `Random (Random (Measure1 a))` represents a `Measure1 a`
  two ways: either `avergae . fmap average`, or `average . join`.
  These should be equal.

### Abstraction

What did we gain by this song and dance of saying that a random `a`
can be viewed as a random one-point measure on `a`?  After all, either
may be queried by a function `a -> Double`, either query's accuracy
may be improved by averaging replicates, and indeed the resulting
computational process will be the same.  My answer is that we gained a
composable abstraction boundary.  A `Measure1` is interpretable only in
light of a query whose expectation is desired.  A `Random` on some concrete
type, on the other hand, is meaningful in its own right, because we
can compute with its samples.

To say what I think is the same thing more concretely, samplers
compose at no computational cost.  If one has a sampler over `a`, and
a dependent sampler over `b` (i.e., a function `a -> Random b`), one
can combine them just by sticking the output of the one into the input
of the other:

```
compose :: Random a -> (a -> Random b) -> Random b
compose sample_a kernel = do
  a <- sample_a
  kernel a
```

Composing measures, on the other hand, requires taking nested
expectations,[^aficionados]

```
compose :: Measure1 a -> (a -> Measure1 b) -> Measure1 b
compose m kernel = Measure1 (\f ->
  expect m (\a -> expect (kernel a) f))
```

and managing the multi-level intractability is bad enough to prefer
the feint to samplers even if a measure is what one really wants.

[^aficionados]: For the Haskell aficionados in the audience, I am
taking "forms a monad" as my definition for "is composable".  Samplers
form a monad in the obvious way.  Monadic composition of measures can
be reified sticking to the `Measure` class as well, but I wanted to
keep this post more accessible than putting GADTs into the main text
would entail.

    ```
    data FreeMeasure b where
      Return :: (Measure m b) -> FreeMeasure b
      Bind :: (Measure m1 a, Measure m2 b) => m1 -> (a -> m2) -> FreeMeasure b

    instance Measure (FreeMeasure b) b where
      expect (Return m) f = expect m f
      expect (Bind m kernel) f = expect m (\a -> expect (kernel a) f)
    ```

### Parallelism

Sampling is embarrassingly parallel.
...

```
data Sample a = Sample [a]
type Sampler a = Random (Sample a)

instance Monoid (Sample a) where
  mempty = Sample []
  mappend (Sample as1) (Sample as2) = Sample (as1 ++ as2)
```

To make querying and computing averages embarrassingly parallel as
well, we need to pull one trick: instead of trying to accumulate the
average, accumulate the total and the number of trials and divide at
the end.  To that end, we need to
- define a representation for the intermediate result of an averaging
  in progress,
  - with an associative combining operation
- Adjust our notion of measures to emit such intermediates, and
- Define how to extract the result

```
data Avg a = Avg !a !Double -- Like Sum

instance (Monoid a) => Monoid (Avg a) where
  mempty = Avg mempty 0
  mappend (Avg tot1 wt1) (Avg tot2 wt2) = Avg (tot1 `mappend` tot2) (wt1 + wt2)

newtype Measure2 a = Measure2 { query :: (a -> Double) -> Avg (Sum Double) }

expect :: (Num a) => Avg a -> Double
expect (Avg tot wt) = tot / fromDouble wt
```

Just for completeness, we can define a monoid instance for `Measure2`
as well (which is actually pretty boilerplate, because it's the
pull-back along `flip query f` of the monoid on `Avg`.

```
instance Monoid (Measure2 a) where
  mempty = Measure2 (const mempty)
  m1 `mappend` m2 = Measure2 (\f ->
    (query m1 f) `mappend` (query m2 f))
```

mumble

```
measure2 :: Sample a -> Measure2 a
...
```

And, isn't this nice, `measure2` and `flip query f` are monoid
homomorphisms (the latter by construction).

What does the representation property call for in this context?

### Summary

So there you have it.  A sampler is a box that produces examples, and
also represents the measure of a probability distribution.  One can
estimate expectations, with a work-accuracy tradeoff through the Law
of Large Numbers, which is embarrassingly easy to parallelize.  A
sampler also serves as an abstraction boundary, by separating the
production of examples from their reduction via a query to something
that can be averaged.  Are there any different things like that?

Weighted Samplers
-----------------

Whenever there are averages, there is a standard generalization to
weighted averages.  Representing a probability distribution by a
machine that draws examples is no exception.  We can broaden the
space of representations we are willing to entertain by allowing the
machine to produce samples from the wrong distribution, but fix up our
inferences by telling us how much attention to pay to any given one.
These sorts of beasts arise, for example, in the [importance
sampling](https://en.wikipedia.org/wiki/Importance_sampling) family of
algorithms.  I will first specify the meaning of a weighted sampler
concretely, and then compare them to regular samplers.

```
newtype WSample a = WSample [(a, Double)]

type WSampler a = Random (WSample a)

w_measure2 :: WSample a -> Measure2 a
w_measure2 (WSample pairs) = Measure2 (\f -> Avg tot tot_wt where
    tot = sum $ [wt * f a | (a, wt) <- pairs]
    tot_wt = sum $ [wt | (_, wt) <- pairs])
```

```
instance Monoid (WSample a) where
  mempty = WSample []
  mappend (WSample as1) (WSample as2) = WSample (as1 ++ as2)
```

`w_measure2` is a monoid homomorphism again; weighted samplers represent
measures and can compute expectations in parallel.

### No static guarantees

yada yada (formalism is weaker, and that cuts
both ways -- there are more ways to form them, they may be faster, but
convergence guarantees are harder / weirder (but, query dependent...)).

### Resampling

is relevant now, and can be done rolling

TODO: Motivate resampling as a computational device (akin to
parallelism!).  It directs downstream computation upon more useful
examples (if the downstream is itself random, multiple copies are
meaningful, because they trigger different explorations (from the same
starting point)).

resample :: Int -> WSample a -> Random (WSample a)

- The output represents the same measure as the input; the Int can be
  used to control the fidelity/cost of downstream computations.

- To represent the same measure, resample needs to conserve the average
  weight.  Other than that, the new weights can be set arbitrarily, as
  long as they are independent of the values (which is an instance of
  the general random weight perturbation).  All equal is probably
  optimal in some sense.

  - For what it's worth, it would be fair to account for only some of
    the weight in the initial sample and leave the rest in the output,
    but why do that?

- resample 2 m = liftM2 mappend (resample 1 m) (resample 1 m)
  holds if resample conserves the _average_ weight
  - Is this law compatible with associativity?  Reasoning by the latter,
      rhs >>= resample 1 ==
      resample 1 (m `mappend` m)
    Should that be the same as liftM (resample 1) (resample 2 m) ?

- resample 1 should distribute over mappend.  That is,
  (resample 1 m1) `mappend` (resample 1 m2) should represent the same
  measure as resample 1 (m1 `mappend` m2).

- That's a (possibly) stronger statement than associativity of resampling:
    resample 1 (m1 `mappend` m2) == 
    liftM2 mappend (resample 1 m1) (resample 1 m2) >>= resample 1
  which holds if resample conserves _total_ weight.
  (Example to consider: m1 is 50 things of weight 1, and m2 is 1 thing of
  weight 50.)
  - Interestingly, resample on `Sample a` has the same bug, but in
    that setting, if both sides represented the same measure, the
    result does anyway.
  - Also interestingly, if m1 and m2 came from the same WSampler, and
    this pattern of resamplings and call counts was independent of the
    values and the weights, then the result should represent the same
    measure regardless.

- resample 1 should preserve queries, up to expectation.
    expect $ query f $ w_measure2 m1 should be the average value of
    fmap (expect . query f . w_measure2) (resample 1 m1)
  This holds regardless of what weight resample 1 puts on the particle.
  - However, for this to be true with w_measure2a, resample 1 needs to
    conserve the _average_ weight.
  - Ditto w_measure3

- The above two laws probably imply that resample conserves mappend
  after query
    expect $ (query f $ w_measure2 m1) `mappend` (query f $ w_measure2 m2) is the average value of
    fmap expect $ liftM2 mappend (fmap (query f . w_measure2) (resample 1 m1))
                                 (fmap (query f . w_measure2) (resample 1 m2))

- Candidate: resample on HWSample chooses the representative in
  proportion to the weight (still) and conserves the total weight
  and total heft in the result.
  - Need only conserve the weight/heft ratio, i.e. the size, for
    preserving queries.
  - Conserving the _average_ weight and heft gives the resample 2 law.
  - Conserving the _total_ gives associativity but breaks resample 2

Rejection sampling
------------------

s_rejection :: Sampler a -> (a -> Bool) -> Sampler a

w_rejection :: WSampler a -> (a -> Bool) -> WSampler a

Commutes with interpreting a Sampler as a WSampler up to downstream
cost; latter can be fixed by applying resample in an appropriate
pattern.

Unnormalized measures
---------------------

We now have another generalization we can make, which is to allow our
weighted samplers to represent un-normalized measures (which will
support an explicit normalization operation that recovers the
probability measure semantics).  Why might this be useful?  TODO Bayes
model selection, general elegance.

With our existing `Measure2` abstraction, that might look like this
(note that expectations are effectively scaled by the size of the
measure):

```
w_measure2a :: WSample a -> Measure2 a
w_measure2a (WSample pairs) = Measure2 (\f -> Avg tot len where
    tot = sum $ [wt * f a | (a, wt) <- pairs]
    len = length pairs) -- Now count the number of trials

size_2a :: Measure2 a -> Avg (Sum Double)
size_2a m = query m (const 1)
```

Unfortunately, computing a normalized expectation from an
un-normalized measure requires two queries now, which defeats
streaming use.  Fortunately, `w_measure2a` is still a monoid
homomorphism.  Unfortunately, normalization is not---if we normalize
a `Measure2`, it forgets its former size, which prevents it from being
averaged successfully.

We can fix that by tracking an additional piece of auxiliary
information---both the total weight and the number of trials.  I'm not
sure what to name the number of attempts, so I will use "heft", for
"another kind of weight".

```
data AvgHeft a = AvgHeft !a !Double !Double

instance (Monoid a) => Monoid (AvgHeft a) where
  mempty = AvgHeft 0 0 0
  mappend (AvgHeft tot1 wt1 sz1) (AvgHeft tot2 wt2 sz2) =
    AvgHeft (tot1 `mappend` tot2) (wt1 + wt2) (sz1 + sz2)
```

We need to update our definition of measures to emit these
objects now

```
newtype Measure3 a = Measure3 {
  query3 :: (a -> Double) -> AvgHeft (Sum Double) }
```

and we can construct these more-information-preserving measures
from `WSample`s like this:

```
w_measure3 :: WSample a -> Measure3 a
w_measure3 (WSample pairs) = Measure3 (\f -> AvgHeft tot tot_wt len where
    tot = sum $ [wt * f a | (a, wt) <- pairs]
    tot_wt = sum $ [wt | (_, wt) <- pairs]
    len = length pairs)
```

The expectation in a `Measure3` is still the average value
```
expect3 :: (Num a) => AvgHeft a -> Double
expect3 (AvgHeft tot _ heft) = tot / fromDouble heft
```

The size of a queried measure is the average weight
```
size :: AvgHeft a -> Double
size (AvgHeft _ wt heft) = wt / heft
```

And we can normalize after querying by scaling the answer and the
weight so that the size ends up being 1.

```
normalize :: (Num a) => AvgHeft a -> AvgHeft a
normalize (AvgHeft tot wt heft) = AvgHeft (tot * heft / wt, heft, heft)
```

This normalization operation still isn't a monoid homomorphism, but
now that it doesn't require the original measure, we can query in
parallel and streaming, and wait to normalize until the end.

If we repeat the `Monoid` instance from `Measure2` for `Measure3`,

```
instance Monoid (Measure3 a) where
  mempty = Measure3 (const mempty)
  m1 `mappend` m2 = Measure3 (\f ->
    (query m1 f) `mappend` (query m2 f))
```

we find that `w_measure3` and `flip query f` are again monoid
homomorphisms.

TODO: Mention that the obvious `s_measure3 :: Sample a -> Measure3 a`
works too?

### Resampling again

Is it possible to choose resampled weights in a WSample to conserve
both the expectation and the size under querying?
- To conserve expectation, need to conserve average weight.

- TODO: Rejection can report approximate size

Another stab at full generality
-------------------------------

Goals:

- Answer the question "What kind of a thing is a sampler, and what
  other things like that are there?"

- Corollary: What information should Venture's particle set
  abstraction maintain?

Outline v3:
- Samplers
  - Measure1 : expect -> Double
  - Abstraction
    - Without the type classes, I can actually write these as actual
      monad instances now
  - Sample [a] is a monoid, so can compute in parallel
  - Move to being able to reduce in parallel too: Measure2 : query -> Avg
    - Rename "finish" to "expect", since that's what it is
    - Avg is a monoid
    - Now we can write the monoid instance on Measure2 directly
      - But it's not very interesting -- just the pull back
    - All three monoid instances agree
  - That's what kind of thing a sampler is
- Weighted samplers
  - Can reuse Measure2 for representing normalized measures
  - No static guarantees, yada yada
  - resample is a new computational techinque applicable here; it works
  - Example: rejection sampling works, can be viewed as either an unweighted
    sampler with a loop or a weighted sampler with rolling resample
- Now we can also represent unnormalized measures = pull out the
  normalization step
  - Useful for, e.g., model evaluation
  - If we tried to use Measure2, normalization would not commute with
    reduction, so Measure3 (w_measure2a also breaks resampling)
  - N.B.: Expectation is scaled by the size of the measure, which
    registers as kinda weird
  - Does resampling still work?

Does HWSample arise as an intermediate representation for resampling?
How about if one is trying to preserve the size of an unnormalized
measure?

Goal: monoid instances at all three levels (sample, measure, result)
should be compatible.

Other proof obligations:
- All the monoid instances should commute.
- Rejection sampling should work as a WSampler (i.e.,
  normalizing that form should have the same effect as doing the reject loop
  and treating that as a sampler).
- Rolling resample should work (i.e., randomly represent the same measure as mappend,
  but for less downstream computation) (also, commutative and associative)

Q: Could I move the weighting into the averaging operation, from the
query?  (Then expect doesn't divide.)
A: Technically, yes, but then a tree of averages is constantly
multiplying and dividing by the weight.  Should be numerically safe,
so I may want to mention it as an alternative.

Idea: Generalize `Measure` to
```
newtype Measure container a = Measure {
  query :: (a -> Double) -> container a
}

measure :: (Monoid container, Functor container)
           => [container a] -> Measure container a
measure as = Measure (\f ->
  reduce mempty mappend $ map (fmap f) as)

-- Except that for this to actually work, the query function does need
-- to bring a monoid instance for the output, b/c Avg's monoid
-- instance depends on that.  So (a -> Sum Double), or some generalization.

type WSample a = [Avg a]
type HWSample a = [AvgHeft a]

-- Also, in that setting, it is not possible to write `w_measure2a`
-- or probably `measure1`.  Perhaps this is a feature?
-- (`w_measure3` only works by first mapping into HWSample)

-- Looking at this, one might be tempted to ask what the `Measure` abstraction
-- is doing here.  One answer is that it already contains the commitment
-- about how a query will be interpreted (averaging, normalizing by weight, etc)
-- but still doesn't have the query.
```
Maybe this goes at the very end?

Another way to do it
--------------------

Instead of `Measure2`, I could manipulate `Avg (Measure1 a)`.
- Then the monoid operation on measures becomes just summation, and I
  use the `Avg` construct to carry the side information.

- `expect :: Avg (Measure a) -> (a -> Double) -> Double` would need to
  divide.

- But I shouldn't have it, because distributed querying was part of
  the point.  The latter is accomplished by `fmap (query f)`.

- This theory still doesn't save me from AvgHeft -- I need the extra
  side information to normalize in one pass, and for resample to work.

Prize-winning
-------------


The standard mathematical definition of a probability distribution
over a set $A$ of possibilities is a _measure_ on $A$ of size 1.  In
other words, a device for computing how likely any given "event"
(subset of $A$) is under this distribution.[^measure-theory] The size
1 requirement is the Law of Conservation of Belief: the probability of
all of $A$ is normalized to 1, i.e., certain.  If you're used to
thinking of a probability distribution as a thing that produces
(perhaps unevenly) random outcomes, and this measure stuff registers
as unfamiliar, I will elaborate the relationship in a bit.

[^measure-theory]: Subject to the usual caveat that not all subsets of
$A$ need be evaluable events---measurable sets, sigma algebra, etc.

This requirement has a direct translation into computational terms: a
probability distribution is a measure, and a measure is an object that
has a method that can measure.  Here is a way to spell that
requirement in the Haskell type class system (read: an object of type
`m` may be used as a measure on objects of type `a`):[^haskell]

```
class BoolMeasure m a | m -> a where
  bool_measure :: m -> (a -> Bool) -> Double
```

[^haskell]: If you don't know Haskell, here is a primer on how to
parse that declaration.  `class` is a keyword that introduces what is
called a type class.  The fragment `BoolMeasure m a` means that two
arbitrary types (here named `m` and `a`) may be declared to stand in
the relationship named `BoolMeasure` by providing an appropriate
collection of functions on them.  What functions?  The requisite type
signatures are given after the `where` keyword.  In this case, only
one function is needed, which is named `bool_measure`.  It must
consume one object of type `m` and one function from `a` to Booleans,
and produce one (double-precision) real number.  The fragment `| m ->
a` is a piece of mumbo-jumbo named a "functional dependency", which
declares that any given type `m` may only be declared to be a
`BoolMeasure` with at most one type `a` (and therefore the type
checker may infer `a` from `m` in appropriate circumstances).

For the rest of the post, I want to move to a stronger definition of
measure, because it's more useful, and samplers meet it anyway.  To
wit, I will take measures to be objects that can evaluate integrals
of real-valued functions with respect to themselves.

```
class Measure m a | m -> a where
  expect :: m -> (a -> Double) -> Double
```

The expectation definition is stronger because the measuring version
is a special case (by computing expectations of indicator functions),
whereas to recover the expectation version from the measuring version
one needs to perform [Lebesgue
integration](https://en.wikipedia.org/wiki/Lebesgue_integration).

Note: The total size of a measure `m` is the size it gives to the
whole space, namely `expect m (const 1)`.  For `m` to be a probability
measure, this must equal 1.  If this is not 1, but is some other
positive real number, `m` may be called an "un-normalized" (finite)
measure.  In this case, if we know what the size of `m` is, we can
convert it to a probability measure by dividing through by its size.
However, that normalization operation loses information (namely, the
size of `m`).  Since that information may be useful (we will see
examples), we will attend to preserving it.

Samplers
--------

Sadly, many interesting probability distributions cannot be
represented exactly by finite computations.  If the distribution `m`
assigns positive probability to infinitely many objects, it is not
possible to define an `expect` that will compute arbitrary
expectations with respect to `m` correctly and in a finite amount of
time.  (More practically, the same is true replacing "infinitely" with
"many" and "finite" with "tractable".)

There are several approaches to dealing with that problem.  Some
expectation questions against some measures can be answered
analytically (perhaps via a computer algebra system).  In other cases,
numerical quadrature methods may be effective (especially if the
object space `a` has few continuous dimensions and not much discrete
structure).  As you might have guessed, I will approach the
expectation problem from the angle of Monte Carlo integration, and try
to motivate using samplers as a canonical representation of measures.

First, what is a sampler?  A sampler for objects of type `a` is just a
box that, every time it is invoked, produces a random `a`.  More
explicitly, a sampler is a computable random variable over `a`, to wit
a function from some inexhaustible supply of entropy (conventionally
named `Omega`) into the space `a`.

```
type Sampler a = Omega -> a
```

A sampler represents a probabilty measure, computing expectations by
Monte Carlo integration.


Option
------

Deal with the intractable infinite measure problem by stepping back to
randomness: `(Measure m a) => Random m` represents the expected
measure.

- Do I want to talk about a generic Sum measure?  Needs to keep track
  of the number of summands to do Monte Carlo integration properly

- Could do this segment as a "what just happened?" after the sampler
  segment.

Break
-----

A sampler for objects of type `a` is just a box that, every time it is
invoked, produces a random `a`.  More explicitly, a sampler is a
computable random variable over `a`, to wit a function from some
inexhaustible supply of entropy (conventionally named `Omega`) into
the space `a`.

```
type Sampler a = Omega -> a
```

A sampler represents a probabilty measure.  The size of a set $A$
under the measure given by a sampler is the probability (under choice
of entropy $\omega$) of landing inside $A$.

A sampler for $\mu$ pulls the problem of computing expectations under
$\mu$ back to the problem of computing expectations under $\Omega$.

How does this representation jibe with the expectation-computing
`Measure` class above?  I'm going for Monte Carlo integration here,
but I'll take a somewhat roundabout way to get there.

We can view a (finite) set of samples as a measure in its own right:

```
newtype Samples a = Samples [a]

instance Measure (Samples a) a where
  expect (Samples as) f = average $ map f as
```

One way to view a sampler, then, is as a device that produces random
measures of type `Samples a`.

```
measure_of :: (Sampler a) -> Random (Samples a)
measure_of as = do
  a <- as
  return (Samples [a])
```

This measure-valued random variable represents the same probability
measure the sampler represents---the operation of measuring sets or
taking expectations is folded into the returned measures, and their
expected value is the represented probability measure.

This move to implicit representation by randomness may look somewhat
unsatisfying.  How useful is a one-point representation of some
complex probability distribution?  Fortunately, we can trade
computation for more usable answers by drawing multiple samples and
combining them:

```
instance Monoid (Samples a) where
  mempty = Samples []
  mappend (Samples xs) (Samples ys) = Samples (xs ++ ys)
```

For sufficiently polite queries, the Central Limit Theorem guarantees
that we will, in fact, gain accuracy for this increased computational
cost.  (Namely, if the variance of `liftM (flip measure f) (measure_of
sampler)` is finite.)

What's the opportunistic abstraction of this?

```
clt_rep :: (Measure m a, Monoid m) => Int -> Random m -> Random m
clt_rep n ms = draw n iid samples and cons them up with the monoid
```

There's a performance-relevant modification possible to this.  If we
have the query function `f` in hand, we can evaluate it in parallel
too, but to do that we may need to maintain additional information.

```
class ParallelMeasure m r a | m -> a, m -> r where
  par_expect :: m -> (a -> Double) -> r
  extract :: r -> Double

data Results = Results !Double !Int

instance Monoid Results where
  mempty = Results 0 0
  mappend (Results tot1 ct1) (Results tot2 ct2) = Results (tot1 + tot2) (ct1 + ct2)

instance ParallelMeasure (Samples a) Results a where
  par_expect (Samples as) f = Results tot ct where
    tot = sum $ map f as
    ct = length as
  extract (Results tot ct) = tot / ct
```

TODO: If `m` has a monoid instance, we expect monoidal combination on
`m` to be compatible with monoid combination on `r` for any given
query `f`.

Weighted Samplers
-----------------

Implement all the above classes, etc.


Another alternative outline would be to punt all the Haskell and type
classes all the way to the end, if at all, and just use words:
- Can estimate expectations
- With controllable accuracy
- In embarrassingly parallel
- And provides an abstraction barrier
and just note that weighted samplers do too.  Maybe have a section
where code appears making it concrete, but put all the code together
(so it's easy to skip).
- Getting into the rolling effort count is useful for implementing
  rolling resample.  Resampling is well-defined but useless for exact
  samplers, but can save downstream computation for weighted samplers.
- Then note that harmonically weighted samplers are another instance
  - Q: But, they only seem to be interesting if the size of the measure
    is to be preserved?
  - A: No, they arise from rejection in that circumstance, but are perfectly
    well defined and sensible otherwise.
  - Q: Wait, can't you just return (bottom, 0) on reject?
  - A: If you want to preseve the size of the measure, you have to
    count the number of attempts separately from the total weight.
  - I think the harmonically weighted sampler arises from rolling
    resample of the WSampler representation of rejection sampling.

Outline:
- Define a probability distribution as a sampler
- Get to expectations and the measure theoretic definition
- Parallel expectation
- WSamplers, etc.  

Outline:
- Define a probability distribution as a measure
  - Note that un-normalized measures may be useful
- Fall back to a random measure as a representation of a measure that
  cannot be represented exactly and deterministically
- Define a sampler as an intermediate
- Derive a tighter approximation class
- Maybe add the Aggregable thing here?
- Note that WSamplers fit
  - Do WSamplers with random weights just fit?  Do I want the aside about them?
- Motivate and note that HWSamplers fit
- Put in BWSamplers for fun?
Prize-winning:
- Fall back to an approximation class

TODO: Can I rewrite all the nonsense to get rid of the classes
completely and just represent a measure as a box around its
expectation function?

TODO: Could refactor all the code to define Sampler a = Random (Sample a).
That way, it's directly a random measure, without the measure1 function.
Composition becomes mildly longer, but whatever.
- Alternately, I could duplicate code for accumulation of measures
  (instance Monoid (Sample a), etc), and then jointly generalize it
  later.

TODO: Compile all the snippets

TODO: Disclaim and/or teach the Haskell class system.

TODO: Note that expectation and sampling are formally equivalent given
infinite computation

TODO: Note that negative or complex weights are allowed.  A non-zero
zero-size measure is, by addition, a size-preseving measure
transformation.  (In other words, a difference of measures of the same
size.)

TODO: Get rid of the question mark in the URL.

TODO: Pull quotes.

TODO: If there are no examples of why un-normalized measures are
useful, rewrite the motivation for them.

Prize-winning entry on approximate measures
-------------------------------------------

The most general thing I can think of that might be called an
"approximate measure" is some function that accepts an integer "level
of effort" and produces a measure[^more-haskell]

```
type ApproxMeasure m a = (Measure m a) => Int -> m
```

[^more-haskell]: The `type` keyword introduces a type synonym.  The
left hand side of the equality is the type being defined, and the
right hand side is the definition.  The thick arrow `=>` whould be
read as "implies" (in contrast with the thin arrow `->` which means
"computes"); all together, that snippet says "If the type `m` is a
`Measure` over type `a`, then a function from Int to `m` is an
`ApproxMeasure` over `a`."

We can say that a particular `f :: ApproxMeasure m a` represents a
measure $\mu$ if the limit as $n \rightarrow \infty$ of `f n` tends to
$\mu$ (for instance, in Kullback-Leibler divergence).

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
