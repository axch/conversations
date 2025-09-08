---
title: Stochasticity <em>is a</em> Quantifier
date: 2014/05/13
author: Alexey Radul
style: "<link href='https://fonts.googleapis.com/css?family=Open+Sans:300&subset=latin,latin-ext' rel='stylesheet' type='text/css'>"
---

In English, quantifiers are words like "all", "one", or "some" that
indicate how broadly true the quantified clause is.  Formal logic has
adopted symbols for such words, namely "all", "exactly one", and
"some" (in the sense of "at least one").  Probability theory offers us
a reason to incorporate another symbol, READMORE with a meaning along
the lines of "some, and I have a sense of how to find them".

I want to present the notation, justify it as a coherent and uniform
extension of classic logical practice, and use it to explain something
I understood much less clearly before: the nature of the distinction
between frequentist and Bayesian statistics, specifically on the
example of comparing confidence intervals to credibility intervals.

Contents
--------

1. [Formulae](#formulae)
    1. [From Logic to Games](#from-logic-to-games)
    2. [The Random Player](#the-random-player)
    3. [Back to Logic](#back-to-logic)
    4. [Summary](#summary)
2. [Statistics](#statistics)
    1. [Frequentist](#frequentist)
    2. [Bayesian](#bayesian)
    3. [Comparison](#comparison)
3. [Reflection](#reflection)
4. [Notes](#notes)

Formulae
--------

### _From_ Logic _to_ Games

[Logic can be embedded into game theory](https://en.wikipedia.org/wiki/Game_semantics).  A
(closed) logical formula with quantifiers (in [prenex form](https://en.wikipedia.org/wiki/Prenex_normal_form)) can be taken to be
a two-player
zero-sum game where I choose a value for every exists-quantified
variable and my adversary chooses
a value for every forall-quantified variable.  I win if
the quantifier-free part ends up being true,
and the adversary wins if the quantifier-free part ends up being false.
The order of making choices and the information available to each
player has to follow quantifier scope.  In this embedding, we call
a quantified formula "True" if I win this game under perfect play, and
"False" if the adversary does.[^scope]

[^scope]: The embedding can be generalized to formulae that are not in
prenex normal form.  Just treat each quantifier as a move by a
player whose goal is to make the expression in that quantifier's scope
come out true (for $\exists$) or false (for $\forall$).  The truth is
dependent upon the values already chosen by all quantifiers in scope
at that point (which values the player knows).  I will, however, stick
with prenex formulae in the main text, because they are
easier to think about.

For example, the formula
$$ \forall k \in \Z. \exists n \in \Z. n > k $$
is the game

1.  Adversary chooses an integer $k$ (not knowing $n$)
2.  I choose an integer $n$ (knowing $k$)
3.  I win if $n > k$, adversary wins if not $n > k$.

Since we are playing over the integers, this game is one I can
always win, which is the same as saying that this formula is true.

The order of quantification, or in other words the order of choices in
the game,[^choice-order] matters---the formula
$$  \exists n \in \Z. \forall k \in \Z. n > k $$
is the game

1.  I choose an integer $n$ (not knowing $k$)
2.  Adversary chooses an integer $k$ (knowing $n$)
3.  I win if $n > k$, adversary wins if not $n > k$,

which under correct play the adversary can always win.  (To wit, the
formula is false).  I will not bore you with the argument that this
embedding is exact (it proceeds by induction on the number of
quantifiers in the formula).

### _The_ Random Player

So far, so good.  What does this have to do with probability, you ask?
Well, game theorists have noticed that it can be useful to allow a
different kind of player in their games---one that behaves randomly
instead of trying to optimize some payoff like the other players.  This
random player is often called Nature (exercise for the reader: why
does game theory never need more than one random player?) and the
probability distribution(s) governing Nature's behavior are taken to
be part of the definition of the game (just like the legal move sets and
objective functions governing the behavior of the normal players).

For instance, the card game
Bridge fits into this framework: at the beginning, Nature makes a move
dealing the cards (which is traditionally taken to be a uniformly
random choice among all possible deals), then each of the four players
observes a portion of the deal (to wit, their own hand), and they
start making moves according to the (now deterministic) rules of
Bridge.  After the bidding, three of the players observe some more of
the deal (the dummy's hand), and then continue making strategic moves.

[^choice-order]: When we say "order of choices", what we are actually talking about
is the information available to a decision maker about the results of
other decisions in the game.  Chronology is a potent metaphor for
capturing one pattern of information flow, namely complete knowledge
about choices made "in the past" and complete absence of knowledge
about choices that remain to be made "in the future".
Non-chronological information structures are possible, however.  For
example, in a three-player game, A might make some move, then B might
make some move knowing what A did, but then C might have to move
knowing what B did but not knowing what A did (except to the extent
that it can be inferred from B's activities).

    Logic generally does not try to encode such patterns, perhaps
because they tend to make the games even more difficult to solve.  One
pattern has been recognized by some authors, however: a "branch
quantifier" is when the adversary and I are to make "simultaneous"
choices, each not knowing what the other has chosen.

### Back _to_ Logic

We can bring the Nature player back to logic.  I
propose using the symbol $\st$ (a backwards letter 'S', for Stochastic) as a quantifier for randomly
chosen variables.[^colophon]  Just like you have to say what set an $\exists$ or
a $\forall$ are drawn from, you have to say what probability
distribution an $\st$ is drawn from.  The usual quantifier order and
scope rules apply.

[^colophon]: Colophon: The glyph $\st$ is the capital [reversed
S](https://en.wikipedia.org/wiki/%C6%A7).  That letter is called `LATIN CAPITAL LETTER TONE TWO` in
Unicode (code point 423), and has `&#423;` for a numeric HTML entity reference.

As to semantics, let us say that a
(probabilistic) formula is "true with probability $\geq p$" if I win
the corresponding two-and-a-half player zero-sum game (the random
player is counted as half) with probability $\geq p$ under optimal
play.

For example, calling a flip of a fair coin looks like
$$  \exists k \in \{\textrm{H},\textrm{T}\}. \st x \sim \textrm{uniform}\{\textrm{H},\textrm{T}\}. x = k, $$
which is the game

1.  I choose $\textrm{H}$ or $\textrm{T}$
2.  Nature flips a (fair) coin
3.  I win if I chose what Nature flipped.

This formula can reasonably be taken as "true with probability 50%".

In the case of fair coins, it doesn't matter who calls it.  The formula
$$  \forall k \in \{\textrm{H},\textrm{T}\}. \st x \sim \textrm{uniform}\{\textrm{H},\textrm{T}\}. x = k, $$
which is the game

1.  The adversary chooses $\textrm{H}$ or $\textrm{T}$
2.  Nature flips a (fair) coin
3.  I win if the adversary chose what Nature flipped,

is also "true with probability 50%".

Observe that each of these is a very different game from the ones where the
chooser gets to see the result of Nature's flip before making their
choice:
$$  \st x \sim \textrm{uniform}\{\textrm{H},\textrm{T}\}. \exists k \in \{\textrm{H},\textrm{T}\}. x = k $$
is the game

1.  Nature flips a coin
2.  I choose $\textrm{H}$ or $\textrm{T}$ (knowing the result)
3.  I win if I chose what Nature flipped.

I can always win this game, so this formula is true (with probability 100%).

Conversely,
$$  \st x \sim \textrm{uniform}\{\textrm{H},\textrm{T}\}. \forall k \in \{\textrm{H},\textrm{T}\}. x = k $$
is the game

1.  Nature flips a coin
2.  The adversary chooses $\textrm{H}$ or $\textrm{T}$ (knowing the result)
3.  I win if the adversary chose what Nature flipped.

The adversary can always make me lose this game, so this formula is
false (i.e., true with probability 0%).

I hope this example has convinced you that when probability occurs in logic, one
must take the same care about scoping quantifiers as one does when
mixing $\exists$ with $\forall$.

### Summary

- $\forall x \in X$ means the adversary chooses $x$ from the set $X$.
- $\exists x \in X$ means I choose $x$ from the set $X$.
- $\st x \sim P$ means an impartial player chooses $x$ according to the
  probability distribution $P$.

Statistics
----------

Now what does this have to do with Bayesian and frequentist
statistics?  Bayesian statistics always reasons about probability distributions,
but frequentist statistics makes definitions with foralls in them.  In other
words, Bayesians play solitaire against Nature, whereas frequentists
take on strategic adversaries.  This means that frequentism is both
harder and more pessimistic than Bayesianism.[^history]

[^history]: The reason for the name "frequentist" is that this is the
kind of statistics one is forced into if one subscribes to the
frequentist justification for probability theory.  In a nutshell, the
frequentist philosophical view is that probability theory legitimately
describes only situations that correspond in a reasonable way to
repeatable experiments with variable outcomes, where the probabilities
are the frequencies (hence the name) of observed results.  Probability
therefore cannot, on this view, be applied to unique situations such
as the true value of some parameter of interest.  Given that the
parameter is nonetheless unknown, one resorts to reasoning about what
one can say for all possible values of the parameter.

    Bayesian statistics, in contrast, relies on the more permissive
view (now associated with the 18th-century philosopher
Thomas Bayes, hence the name) that probability theory is an extension
of logic to propositions whose truth is not known with certainty.
Under this view, there is nothing wrong with treating things like
fixed but unknown parameters probabilistically, so no foralls are
necessary.

I will illustrate by comparing [confidence
intervals](https://en.wikipedia.org/wiki/Confidence_interval) and
[credible intervals](https://en.wikipedia.org/wiki/Credible_interval),
the textbook frequentist and Bayesian, respectively, approaches to the
[interval
estimation](https://en.wikipedia.org/wiki/Interval_estimation) problem.
Interval estimates are the sort of statistics one sees in the news:
there is some number of interest, such as the proportion of voters
that lean towards one or another political party in an upcoming
election; some evidence about this number is gathered, such as polling
a random fraction of those voters; some computation is done; and an
interval is announced, that is purported to contain the number of
interest with some degree of confidence.  As we shall soon see, the
frequentist and Bayesian notions of "confidence" are actually quite
different; but that is the basic set up.

### Frequentist

The standard frequentist tool for inverval estimation is the
confidence interval.  More generally, one can define confidence regions
for set estimation of parameters that have other structure than single
numbers.  To give the construction, we start with a little notation:

- Suppose the item we are interested in is drawn from a _parameter
  space_ $A$.

In a simple rendition of the political example, this would be $[0,1]$,
representing all possible fractions of voters preferring one
particular party over the other.

- Suppose the experiment we conduct produces results in some
  _observation space_ $B$.

In the example, the experiment could be a poll, and $B$ could be the
space of possible results.

- We model the influence the parameter exerts on the observations as a
  function from $A$ to $B$, which we take to be random because we
  assume the observations are also affected by other influences.  This
  function is traditionally called the _likelihood_[^likelihood], and
  can also be seen as a deterministic function from $A$ to probability
  distributions over $B$:
  $$\textrm{likelihood}: A \to \Pr(B).$$

[^likelihood]: This function is actually a probability distribution
over $B$ conditioned on a value from $A$.  The reason it's called a
"likelihood" and not a "probability" is because in this use case we
are interested in its behavior over the space $A$, with a value in $B$
held fixed.  Holding $b$ fixed, it measures how
good---"likely"---various $a$ look, but it does not give a probability
distribution over $A$.

In the polling example, the randomness of the $\textrm{likelihood}$
would include, for instance, our choice of whom to poll.

The definition of the likelihood function is the place where our
qualitative modeling assumptions turn into analyzable objects that we
can do mathematics with.  Now,

- a _95% confidence interval_ is a (deterministic) procedure for
  going from an observation to a set of possible parameters
  $$\textrm{conf_int}: B \to \mathcal P(A)$$
  such that
  $$ \forall a \in A. \st b \sim \textrm{likelihood}(a). a \in \textrm{conf_int}(b) $$ with probability $\geq$ 95%,
where the probability is taken over the randomness of the
likelihood.

In words, this formula means that for any $a \in A$ (chosen to be as
difficult as possible), at least 95% of the $b$ drawn according to
$\textrm{likelihood}(a)$ are such that the original $a$ is inside the
confidence interval computed from the given $b$.  In the political
example, this translates to the following requirement on the
confidence interval procedure: whatever the true leanings of the
population may be, at least 95% of possible polls conducted according
to our design must lead, via our $\textrm{conf_int}$, to intervals that
contain those true leanings.

The thing to remember is that $\textrm{conf_int}$ does not depend on $a$.  As a
game, finding confidence intervals for a given problem looks like

1. I choose a function $\textrm{conf_int}: B \to \mathcal P(A)$
1. The adversary chooses an $a$ (knowing $\textrm{conf_int}$)
2. Nature chooses a $b$, given the adversary's $a$, according to the $\textrm{likelihood}$
3. I win if $a \in \textrm{conf_int}(b)$.

The design task when choosing $\textrm{conf_int}$ is usually to
minimize the cardinality of the sets that it returns, subject to the
above game being won with probability at least 95%.

### Bayesian

The standard Bayesian tool for inverval estimation is the credible
interval.  In general, a _95% credible interval_ for some probability
distribution $\pi$ on some set $A$ is a
set of $S \subset A$ such that
  $\st a \sim \pi. a \in S$
with probability $\geq$ 95%,
where the probability is taken over the given distribution.  The design
task is usually to minimize the cardinality of the set.

This applies to the interval estimation setting as follows:

- Start with a parameter space $A$, an observation space $B$,
  and a $\textrm{likelihood}: A \to \Pr(B)$ as before.

In fact, the choice of $\textrm{likelihood}$ function for any given
problem is often common between frequent and Bayesian analyses.

- We model our existing, pre-experiment knowledge about our problem as
  a probability distribution $\pi$ on $A$, which is called the
  _prior_.

In the political example, the prior might be the uniform distribution
on the interval $[0,1]$ if we modeled the problem assuming relatively
little knowledge about politics; or it might be a Gaussian
distribution with mean 50% and standard deviation 1 percentage point,
if we modeled the problem assuming pretty strong external evidence
that the election was going to be close.  Choice of prior is
important---different priors mathematically encode different problems,
so yield different answers.

- Given a prior and a likelihood, [Bayes' rule](https://en.wikipedia.org/wiki/Bayes%27_theorem)
  gives the procedure for finding _posteriors_ conditioned on possible observations
  $b \in B$, which are also probability distributions on $A$.
  The application of Bayes' rule can be viewed as a function,
  $$\textrm{posterior}: B \to \Pr(A),$$
  which updates our prior distribution to reflect learning the information $b$.

In the political example, the posterior distribution describes the
state of our knowledge about the coming election after digesting the poll
results.  It's called the posterior (as opposed to the prior) because
it is the distribution post-experiment.

- To get an interval estimation procedure, we can compose computing
  posteriors with choosing 95% credible intervals to get
  $$\textrm{cred_int}: B \to \mathcal P(A).$$
  This $\textrm{cred_int}$ then has the property that
  $$\st a \sim \pi. \st b \sim \textrm{likelihood}(a). a \in \textrm{cred_int}(b)$$
  with probability $\geq$ 95%, where the probability is taken over
  _the prior and the likelihood_.

In words, this formula means that at least 95% of the time, when $a$
is drawn according to $\pi$ and $b$ is drawn according to
$\textrm{likelihood}(a)$, it turns out that $a$ is inside the credible
interval computed from $b$.  The translation to the political example
is direct: in 95% of leaning-poll pairs, where the population leanings
are drawn according to the prior and the poll results are drawn
according to the likelihood, the credible interval computed from the
poll result will contain the true leaning of the population.

One way to render selection of credible intervals as a game is

1.  I choose a function $\textrm{cred_int}: B \to \mathcal P(A)$
1.  Nature chooses $a$ according to the prior
2.  Nature chooses $b$ given $a$ according to the likelihood
3.  I win if $a \in \textrm{cred_int}(b)$.

The design task when choosing $\textrm{cred_int}$ is usually to
minimize the cardinality of the sets that it returns, subject to the above
game being won with probability at least 95%.

### Comparison 

Look at these formulae side by side.  A 95% confidence
interval for a given parameter space $A$ and a given $\textrm{likelihood}: A \to \Pr(B)$
is a function from $B$ to $\mathcal P(A)$ such that
  $$ \forall a \in A. \st b \sim \textrm{likelihood}(a). a \in \textrm{conf_int}_{A,\textrm{likelihood}}(b) $$
with probability $\geq$ 95%.

A 95% credibility interval for a given prior $\pi$ over
$A$ and a given $\textrm{likelihood}: A \to \Pr(B)$ is a function from $B$ to $\mathcal P(A)$ such that
  $$\st a \sim \pi. \st b \sim \textrm{likelihood}(a). a \in \textrm{cred_int}_{\pi,\textrm{likelihood}}(b)$$
with probability $\geq$ 95%.

The difference between these two formulations is that the frequentist formula
has a $\forall a \in A$ where the Bayesian one has a $\st a \sim \pi$.  In other words, where
the frequentist analysis assumes an adversary, the Bayesian one postulates a fixed
(probabilistic) behavior.  This has several consequences:

- Frequentist statistics answer a different kind of question from
  Bayesian ones.

- The frequentist question should be _askable_ in situations where the
  Bayesian one is not, namely where the information available about possible $a$s
  cannot be captured as a probability distribution.[^uninformative]

- The Bayesian question should be _answerable_ in situations where the
  frequentist one is not, because having more than one kind of
  quantifier always causes trouble.  Here I mean mathematically
  answerable; empirically there are circumstances where a forall is
  computationally more tractable than a probability distribution.

- When a situation is modelable in both styles, one would expect the
  answer to the Bayesian style of question to be more optimistic than
  the frequentist, because the adversary is assumed to always choose
  the worst possible $a$.  Sometimes, when the prior encodes more
  information than we are actually justified in assuming, optimism can
  lead to incorrect conclusions.  Other times, when "all" possibilities
  admit arbitrarily extraordinary coincidences, pessimism can lead to
  conclusions so weak as to be paralyzing.

- What question, exactly, "the frequentist question" actually is
  depends very strongly on the details of the game design: where the
  foralls/adversaries actually go, and in what order the choices are
  made.  For example, it is important that the confidence interval function is
  to be chosen before the adversary chooses the $a$ at which to test
  it.

    In practice, the choice of how to encode a given complex
  statistical situation as a frequentist adversarial game can be
  just as contentious as the choice of prior in a Bayesian analysis.

Reflection
----------

A quantifier indicates the "quantity" of things
about which something is true.  In this sense, all three of the symbols
that appear in this essay are quantifiers---$\exists$ is "at least one",
$\forall$ is "all", and $\st$ is "several".  The
game theoretic view, however, exposes a distinction
between $\exists$ and $\forall$ as opposed to $\st$.  The former two
are optimization quantifiers: they define the value they bind by
specifying a set of possibilities and an objective function, and
assuming the optimal value for that objective is somehow found.
$\exists$ and $\forall$ differ only in the objective (to make the
subsequent formula true or false, respectively), so even in the game
theoretic sense they are very similar creatures.

The random choice quantifier $\st x \sim P$ is different.  The
$x$ is not an extremum of anything; it is chosen at random.  And yet
$\st$ is also the same as the classical quantifiers, in that
it also hides the details of how $x$ is chosen---this time behind the
probability distribution $P$ (which could be very complicated, and
very difficult to actually select a value from computationally).[^teaching]

I wonder, then, what other
complex processes would it be worth making symbols for?  One
thought is that $\exists$ takes on a slightly different meaning in
intuitionistic rather than classical logic.  I'm a bit fuzzy on the
details, but I guess it corresponds to computable optimization (rather
than the absolute optimization of classical $\exists$).  Does it make
sense to ask for computationally limited optimization?  Something like
a game where the player is permitted only a polynomial amount of
computation after seeing the last move before having to make theirs?

Notes
-----

[^uninformative]: I stress that this situation is rarer than one might
think, because many collections of information are capturable as
probability distributions, without requiring appeal to repeated
experiments with known mechanisms.  In particular, the Bayesian
statistics community has derived priors for many problems with the goal
to "let the data speak of themselves"---to wit, encode no additional
information at all, beyond the modeling assumptions encoded in the
$\textrm{likelihood}$ function.  I encourage the reader to look up
"uninformative priors" or "reference priors".

[^teaching]: Perhaps it should not be surprising that students get
confused by frequentist statistics.  That field treads the relatively
unexplored ground of mixing different kinds of quantifiers in a single
theory.  Besides game theory, it is the only theory I know about that
does so; and since both of them are pretty new, perhaps we haven't
worked out good ways to think about such mixtures, or to teach people
to think about such mixtures.

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    Macros: {
      R: "{\\mathbb{R}}",
      Z: "{\\mathbb{Z}}",
      eps: "\\varepsilon",
      st: "\\unicode{423}",
      sim: "\\propto"
    },
    equationNumbers: { autoNumber: "AMS" },
    noErrors: { disabled: true },
  }
});
</script>
