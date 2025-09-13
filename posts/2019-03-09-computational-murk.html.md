---
title: Computational Murk
date: 2019/03/09
author: Alexey Radul
---

Why is playing chess fun?  Why are people irrational?  What is
randomness?  Why does cryptography work?  What do these questions have
to do with each other?  READMORE

Let's start with chess.  Can you imagine a perfectly rational agent, a
[_Homo Economicus_](https://en.wikipedia.org/wiki/Homo_economicus),
if you will, playing chess?  To be perfectly
rational is to know all the consequences of everything one knows.
Knowing the rules of chess, our _Homo Economicus_---call her
Alyssa---would know who wins (or, more likely in the case of chess,
how to force a draw), would know the array of winning (or drawing)
moves from any given position, and would know that any other _Homo
Economicus_---Ben---knows the same.  Chess, for Alyssa and Ben, would
be a mindless exercise in stepping through one of the perfect games,
where every move is either forced, or an arbitrary choice among a
handful of known options, all leading to the same inevitable,
preordained result.  Alyssa would not play chess.

This caricature is, of course, absurd.  Our civilization's best minds,
using our civilization's best computers, have not managed to play even
one game of chess anywhere near as well as Alyssa.  But why, exactly,
not?  Of course we know: there are too many possible variations from a
given position.  The number of potential consequences of a chess
position grows exponentially in the number of moves ahead one tries to
look, and that exponential quickly blows past all computational abilities.
So what, then, does it mean to make a move in a chess game?  It is a
decision.  Not just any old decision, but a decision specifically
about how to think: moving is deciding to stop analyzing potential
alternatives.  Placing the piece in its new position on the board is
communication: telling one's opponent what one has decided.

<blockquote class="pullquote-display"><p> Between the total clarity of
knowledge and the total opacity of ignorance lies the computational
murk.</p></blockquote>

So what does a game of chess between realistic players look like?
White spends some time thinking about moves and their consequences.
If they're any good, Black is also spending that time thinking about
the same thing.  But, since they're a different person (or a computer
running with a differently-initialized random-number generator), and
since neither White nor Black has enough computation to think through
everything, they think at least somewhat different things.  Then the
move comes, and now Black's clock is ticking.  Unless White is much
stronger than Black, Black can put more computrons into analyzing
White's move after the decision than White could before---the whole
point of it being a decision was that White was spending at least some
computation on alternatives, and moved exactly when they decided not
to spend any more.  That's why it's fun: if the players are close to
the same strength, Black can surprise White, by thinking about White's
move harder (or differently).

So does White know the consequences of their potential moves or not?
The standard model of rationality demands the answer be "yes", since
the position, the rules, and the players' goals are all deterministic
common knowledge, with no information hidden from anyone anywhere.
Yet perfectly clear knowledge of the consequences is impossible,
because no one is that smart.  Is the answer then just "no"?  That
can't be right either---thinking even a little about possible
responses makes one less in the dark about what the other player will
do than one is about a roulette wheel.

Now I have gotten to the point.  Knowing or not knowing is not the
dichotomy it appears to be.  Between the total clarity of knowledge
and the total opacity of ignorance lies the computational murk.  There
are things one knows, in some operational sense.  There are things one
could discover if one reasoned.  But reasoning takes time and blood
sugar (or electricity), so one must always be deciding what to reason
about and how much.  Others may decide differently, and thus come to
different conclusions from the same premises---conclusions one could
in principle understand, but only in return for more blood sugar.  In
some areas this regress seems to have an end, but in chess it's
obvious that knowledge is always bounded by the murk.  Chess is a game
of knowing how (and when) to peer deeper into the mist.

Now that we have it, on what can the idea of computational murk shed,
so to speak, light?

I promised cryptography.  There the murk is so deep as to be near
total darkness.  The goal, when designing ciphers, is to make sure no
one without the encryption key can decrypt---except by guessing the
key.  But the darkness even an ideal cipher generates is not quite
total: testing out a guess takes a finite amount of computation, and there
are only a finite number of possible keys, so no encrypted secret is
safe from a truly computationally unlimited thinker.  It's just
usually unhelpful to model cryptography that way, because the gain
from trying to guess a key is so small as not to be worth the trouble.
Nonetheless, choosing not to waste time guessing private keys is a
decision about how to think: different in parameters but not in kind
from the decision not to think more about a move in chess.

I promised randomness.  Pseudo-random number generators are actually
better modeled as ignorance amplifiers.  One modern example works like
this: Start with some seed, of, say, 32 bytes.  Then encrypt the pair
`(seed, 1)` with a cipher that emits 64 bytes.  Someone who doesn't
know or guess the seed (and can't break the cipher) can't predict
those 64 bytes.  If more bytes are needed, encrypt the pair `(seed,
2)`, then `(seed, 3)`, and so on.  Voilá---little ignorance has become
much ignorance.

But are such pseudo-randomly generated bytes "really random"?  That's
the wrong question.  Their future is hidden by computational murk, as
dark as a bitcoin wallet and no darker.  The future of dice and decks
of cards and roulette wheels is also only as unpredictable as our
unwillingness to measure well enough and simulate kinematics and
turbulence fast enough.  That's what "randomness" is: the choice to
reason about a thing as though it were unknown (i.e.,
probabilistically) instead of trying to wade through the murk needed
to know it.

That choice is made more uniformly in some circumstances than in
others.  The design objective of crypto is to make the murk so dark
that it's safe to bet billions of dollars that no digital thief can
see through it.  In my own professional life, these choices of what to
try and compute through and what to leave in the fog are called
"modeling assumptions", and reasonable people regularly disagree on
how to make them in any given circumstance.  It ends up being a
tradeoff between cost and accuracy.  It gets amusing when the accuracy
in question determines another purely computational decision, and thus
can be brought back to the same units of cost.

One particularly interesting example of this kind of circularity lies
at the foundations of the technology called Bayesian optimization.  Bayesian optimization is
a family of techniques for looking for the maximum of some function
(for example, the test accuracy of a neural network one is training,
as a function of, say, the number and widths of its layers).  The
techniques themselves end up being pretty computationally costly, so
people only tend to use them when the function being optimized is so
expensive to compute that it's really valuable to squeeze as much as
possible from every trial.

What bothered me for a long time about all this was the set of
modeling assumptions.  One begins the math for Bayesian optimization
by saying that one has some unknown function $f$ with some known
input-output pairs $y_i = f(x_i)$.  Then one proceeds to put down some
sort of probabilistic prior on the space of possible functions
(typical choices in this setting just favor smoothness) and to do a
bunch of math to form a posterior probability distribution over $f$
conditioned the known outputs.  Having formed that probability
distribution one then chooses the next point at which to try
evaluating $f$ based on some criterion, such as maximum probability of
exceeding the previous optimum.

But, back up a second here.  Why is it legitimate to model $f$ as
unknown, when we can just compute it?  Pragmatically, of course, the
answer is that we only use Bayesian optimization when the price of all
this modeling rigamarole is less than the price of evaluating $f$ at
the wrong point.  But philosophically, how can we simultaneously treat
$f$ as known enough to compute and unknown enough to model
probabilistically?  How can the value of $f$ at a new point $x'$ be
"unknown" and subject to modeling when the computer running $f$ is
right there and we can just ask it?  I had much cognitive dissonance
working through the math of deriving perfectly rational conclusions
from such clearly incomplete---intentionally irrational?---modeling
assumptions.

Computational murk restores consonance.  We neither "know" nor "don't
know" $f$.  Rather, it's murky.  The source code means we can discover
$f(x)$ for any $x$---but that costs time and, in the cloud computing
setting where these methods are common, money.  To make the best use
of that time and money, we alternate between learning a new value of
$f$ at a carefully selected $x$, and pretending we have no access to
$f$ while thinking about the next $x$ to select.  My former dissonance
now becomes a clue to a possible research direction: Are there static
analyses that could read the source code of a suitable $f$, and derive
some clues about its behavior that could then be folded into a more
informative prior for use in Bayesian optimization?

Which brings me to another instance of computational murk, namely
compilers.  Especially just-in-time compilers.  Much is made in the
compiler world of the distinction between "static" and "dynamic"
information, or "static" and "dynamic" program analyses.  As a
practical matter, "static" stuff is available "before the program is
run", whereas "dynamic" stuff is available only as it runs, and may
vary, for example, from one call of some subroutine to another.  But
as a philosophical matter, one asks why it seems so stark.  After all,
one compiles programs for computing $\pi$; and when one does that, all
the information about everything that program will do is, in
principle, "available to the compiler".  But some of it (like the data
types of internal variables) is somehow more "static" than other (like
actual digits of $\pi$ that flow through those variables).  The
distinction can also be seen as the murkiness of knowledge: yes, in
principle the compiler could know what numbers will appear in what
variables at the 100,000-th step in the computation; but computing that
is too costly, and the ultimate results are better if one thinks just
about data types first.  The compiler's goal is to look through the
murk partially, and construct a program that will look all the way
faster than the compiler itself would.

But enough about economists' and computer scientists' abstractions.
Perhaps by this point you can guess what I will say about what "being
irrational" means when used colloquially about normal people.
Alyssa's perfect rationality is absurd, but there is nonetheless a
culturally accepted ideal of how (and how much) one is expected to
reason about a given situation in light of given information.  To be
"irrational", then, is to reach a different conclusion than this
idealized "culturally rational reasoner" is expected to reach in the
same circumstances.  A necessarily fuzzy concept, because "cultural
rationality" is constructed at best out of the judgements of real
people, through their conversations with each other and in public
about the situation in question; and they disagree often enough.  And
so we have many fuzzy words for various fuzzy boundaries around this
fuzzy concept: being confused, following emotional logic,
overthinking, making mistakes (honest or otherwise).

Let me end on what for me is a positive note.  Computational murk is
real, so irrationality is inevitable.  Individual humans are
irrational.  Human organizations are irrational---they may have more
total computation than any one person, but they also have more they
need to think about (such as all their internal working
relationships), so murk limits and bedevils their effective knowledge
as well.  Human governments are irrational.  Emergent human
institutions (such as "the market") are irrational.  Perforce.  There
isn't enough computation in all the minds and all the computers in all
the world to think through everything that merits thinking through.
So there is room both to question convention and orthodoxy, and,
perhaps, to improve it, by choosing to think about something that the
culture effectively hasn't thought about before.

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
