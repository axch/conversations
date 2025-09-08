---
title: <em>On</em> Intentionally Random Programs
date: 2016/03/20
author: Alexey Radul
---

For a little over two years, I have been professionally dealing with
programs whose behavior is intentionally random.  Why would one even
have intentionally random programs?  READMORE

The first kind of "random" program I encountered was the **randomized
algorithm**, but I do not consider these to be intentially random in
this sense.  Such a program's purpose is to compute something
deterministic, and randomness is a trick employed to be sure the
algorithm avoids all possible bad patterns.

One classic example of this is the [Fermat primality
test](https://en.wikipedia.org/wiki/Fermat_primality_test).  The goal
is to determine whether some large $n$ is prime, which is not a random
thing at all.  The method is to try various $a$ to see whether
$a^{n-1} \not \equiv 1 \mod n$---if at least one such witness is
found, $n$ is certainly composite, and if not, $n$ is "probably"
prime.  The randomization helps in the following way: if $n$ is
composite (and not a Carmichael number, which are much less
common than primes), then at least half of the possible $1 < a < n-1$ will
be witnesses, but there is no theory indicating which ones they will
be.  So we cannot prove that any particular search pattern will quickly
find a witness, but we can prove that choosing candidates at random
will quickly find a witness with high probability.[^pseudo-random]

The second kind of "random" program I encountered is the creation of a
**cryptographic secret** (such as a large semiprime for use in the RSA
algorithm).  I guess key generation really is "intentionally" random,
but it nonetheless has a different flavor from the kinds of
intentionally random programs I have been working with.  The output is
a single sample from the probability distribution on secrets.  The
name of the game is to prevent adversaries from guessing the output,
to which end the distribution is engineered to be as high entropy as
possible, with as little other structure as possible.

What I have been working with, however,
are programs that are intended to produce interestingly random results---programs whose
**probability distribution on results is itself the design
objective**, and subject to arbitrary domain-specific desiderata.  One
nice source of examples is
[recent](https://stanford.edu/~dritchie/procmod-smc.pdf)
[work](https://stanford.edu/~dritchie/graphics-hmc.pdf) in "procedural
modeling": teaching computers to come up with (random) suggestions
for designs, for example for computer generated graphics in movies or
games.  This business gets tricky when the artistic objective has
requirements that are unlikely to be met purely at random: trees that
grow around obstacles, networks of pipes that cast a specific shadow,
space ships or cities that match a given overall shape.

In probabilistic procedural modeling, the goals of randomization are
very different from randomized algorithms or cryptography: the output
is several different instances of random objects of the same
specification; each is expected to more or less meet given artistic
constraints; and they should otherwise be as varied (in a domain-specific sense)
as practicable, to
provide appropriate options for selection or an appropriate base for
inspiration.

Another, more staid, source of examples is the field of Bayesian
statistics, such as the [recently adopted United Nations methodology
for projecting human
populations](https://esa.un.org/unpd/wpp/publications/Files/WPP2012_Methodology.pdf).
In this case, the output of the intentionally random component is
thousands (or millions, for all I know) of candidate population
trajectories.  These trajectories are used to form the published
predictions, and the published error bars on those predictions.

The underlying population model uses probability theory to quantify at
least two different kinds of uncertainty---uncertainty about general
properties of population growth, which is mitigated but not eliminated
by calibrating the model on historical population data, and
uncertainty about how population will actually grow in whatever
(uncertain) specific circumstances arise in the future.  The design
requirement for the computer program that realizes this model is for
the probability distribution of the output trajectories to faithfully
represent the model's residual uncertainty about what will actually
happen.

Why is the United Nations' population program random?  The end-to-end
process, from gathering population data to publishing graphs and data
tables with projections, might be called "unavoidably random".  No
doubt the UN Population Division would prefer to be able to
deterministically compute what numbers to put into the Population
Prospects report to faithfully represent the uncertainty quantified by
their model.  This is even theoretically possible---the 80th
percentile value in their model's probability distribution over world
population in 2057 is some mathematically defined real number.  The
trouble is that this number is defined in terms of horrible
multi-dimensional integrals over all possible unknowns in the model.
For any but the simplest models, these integrals cannot be determined
analytically, and are not tractable to accurately determine by
numerical integration.  The best the UN can do is draw many random
samples from (an approximation to) the model's probability
distribution on trajectories and describe the aggregate, relying on
the law of large numbers to reduce the residual randomness in their
final report to an acceptable level.

The last two examples motivate what **probabilistic programming** is for.
Both procedural graphics and population projections (and hosts of
other applications I haven't mentioned) have components that are
random programs manipulating complex objects, whose space of possible
behavior is the design criterion of interest.  Even when there is a
"final" output whose randomness is either undesirable (such as the
population report) or irrelevant (such as the details of a
good-looking computer-generated snowstorm), the program that produces
it contains internal interfaces where correctness is defined by the
probability distribution on returned samples.  [Probabilistic
programming languages](http://probabilistic-programming.org/wiki/Home)
are for writing such programs (more concisely
and with fewer errors than otherwise), and probabilistic software
engineering needs to be about inspecting, testing, debugging,
optimizing, and maintaining such programs.

References
----------

-  Daniel Ritchie, Ben Mildenhall, Noah D. Goodman, and Pat Hanrahan,
   "Controlling Procedural Modeling Programs with
   Stochastically-Ordered Sequential Monte Carlo", SIGGRAPH 2015.
   Preprint:
   [https://stanford.edu/~dritchie/procmod-smc.pdf](https://stanford.edu/~dritchie/procmod-smc.pdf)

-  Daniel Ritchie, Sharon Lin, Noah D. Goodman, and Pat Hanrahan,
   "Generating Design Suggestions under Tight Constraints with
   Gradient-based Probabilistic Programming", Eurographics 2015.
   Preprint:
   [https://stanford.edu/~dritchie/graphics-hmc.pdf](https://stanford.edu/~dritchie/graphics-hmc.pdf)

 - United Nations, Department of Economic and Social Affairs,
   Population Division (2014).  "World Population Prospects: The 2012
   Revision, Methodology of the United Nations Population Estimates
   and Projections", Working Paper No. ESA/P/WP.235.
   [https://esa.un.org/unpd/wpp/publications/Files/WPP2012_Methodology.pdf](https://esa.un.org/unpd/wpp/publications/Files/WPP2012_Methodology.pdf)

Notes
-----

[^pseudo-random]: In practice, of course, the "random" choices of $a$
are generally pseudo-random, in the sense of being generated from a
small amount of physical entropy amplified by a pseudo-random number
generator (which is a deterministic program designed to be difficult
to reverse-engineer) into a large enough stream of bits to form the
desired $a$s.  In this case, any particular initial seed for the PRNG
does define a specific deterministic search pattern; the argument that
this is nonetheless acceptable turns into one about the PRNG not being
accidentally ill-aligned with the primality testing problem.

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
