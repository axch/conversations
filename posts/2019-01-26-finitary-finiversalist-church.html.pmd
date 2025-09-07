---
title: The Finitary Finiversalist Church
date: 2019/01/26
author: Alexey Radul
epigraph: The study of mental objects with reproducible properties is called mathematics.<cite>&ndash;Davis and Hersh, 1981</cite>
---

Infinity.  What is it, really?  Perhaps you remember, as I do, the
moment in your life when you realized one could (in principle) count
forever, and there would always be integers there for one to count.
Yet, how can our finite minds comprehend the truly infinite?  Do
infinte things even exist?  READMORE

My own thoughts on this subject have gone full circle.  I accepted
the infinitude of the integers at a young enough age.  Since I recall this
as an act of acceptance, I infer that I must previously have assumed
that only finite things exist.  But accept I did, and proceeded,
over the course of my mathematical education, to grow more and more
steeped in the mysteries of the non-finite.  One can measure
infinities by bijection; so the even numbers are no more and no less
infinite than all the integers, and than the positive numbers
divisible by 1000.  Yet the real numbers are more infinite---they are
too many to be counted, for there is no 1-1 mapping between them and
the counting numbers.

It went on from there: One can measure infinities more finely by
order-preserving bijection.  This leads to the
[ordinals](https://en.wikipedia.org/wiki/Ordinal_number), to
[transfinite induction](https://en.wikipedia.org/wiki/Transfinite_induction),
to the [continuum hypothesis](https://en.wikipedia.org/wiki/Continuum_hypothesis).
The [axiom of choice](https://en.wikipedia.org/wiki/Axiom_of_choice)
rears its controversial head.  I was in my
element, an initiate of eldritch powers delving (or, at least,
learning how to delve) into eternal secrets beyond the (finite)
universe.  I accepted the Axiom for the power it gave, and looked down
upon the doubters and detractors as only a teenager can.

But the study of computing sowed the seeds of doubt, and the practice
of probability germinated them, and with the passage of years they
have begun to sprout.  Real numbers are awkward things to try to
compute with.  It may be a little trite to say that you can't store
all of the digits of a real number in the memory of the computer, but
the limits on precision really are a consistent source of trouble in
practice.

Probabilities over real numbers are awkward too.  It's easy enough to
say "consider a real number drawn uniformly between 0 and 1", but you
can't actually draw one: the probability of any given $x$ under that
distribution is 0 (whether $x$ is between 0 and 1 or not!).
Mathematically, one is forced to resort to the usual song and dance
about probability densities: the limit as $\eps$ goes to zero of the
ratio of the probability in the interval to the size of the interval,
yada, yada.  Computationally, finite precision makes this even worse:
the joke in probabilistic computing is that probability zero events
happen alarmingly often.  To say nothing of trying represent those
probability densities, and of periodically being bitten by having
forgotten whether one was referencing
[Lebesgue](https://en.wikipedia.org/wiki/Lebesgue_measure) or
[counting](https://en.wikipedia.org/wiki/Counting_measure)
measure.

So what is it like to sit in the shade of the tree of doubt?  The
epigraph from which I started this essay is the foliage between me and
the burning sun of irresolution.  If mathematics is to be a study of
mental objects, one cannot simply ban infinities, because infinite
mental objects are too easy to construct (they even form a [useful
programming
technique](https://en.wikipedia.org/wiki/Lazy_evaluation#Working_with_infinite_data_structures)).
But they must have _reproducible_ properties: so the place to draw the
line is at objects that take an infinite amount of information to
describe.  This is the article of faith that defines finitary
finiversalism: infinity only as the consequence of a finite description.

What, then, is admissible?  The integers may be infinite, but they are
probably the most reproducible mental object in all of
mathematics---every kid learns the integers.  The rationals are fine
too, as each is given as a ratio of two finite integers---nothing
non-reproducible about that.  What of the reals?  They are
conventionally defined as the completion (in the order topology) of
the rationals.  That is, a real number is the limit of a sequence of
rational numbers.  How are we to reproduce such a sequence?  We can't
just write it all down, because the whole point is that the sequences
with irrational limits are infinite.  But we can of course specify
some sequences.  For instance, "the $k$th term is the perimeter of a
regular $2^k$-gon of diameter two."  The limit is a perfectly good
irrational number, with very many very well-reproduced properties.

Which sequences of rationals are describable with a finite amount of
information?  Well, certainly any given Turing machine is fine: it can
be encoded as a finite string in a finite alphabet.  The only thing
infinite about (some) Turing machines is their behavior; and the
behavior can be reproduced by reproducing the machine.[^input] So any
sequence of rationals that we can program a (finite) computer to print
is reproducible, at least in principle.  And the
[Church-Turing thesis](https://en.wikipedia.org/wiki/Church-Turing_thesis)
tells us that's it.

[^input]: And its input, and the model of computation in which it's
interpreted, but those are finite too.

In fact, the Church-Turing thesis tells us that's it for all of
mathmatics, not just the reals.  Computable objects are the _only_
objects that can be described with a finite amount of information.
This view reassuringly eliminates various controversies in
mathematical foundations:

- Among sets of computable objects, the [well-ordering
  theorem](https://en.wikipedia.org/wiki/Well-ordering_theorem)
  is obvious, because we can always order objects
  lexicographically by (the description of) the smallest Turing
  machines that compute them.[^computable-well-ordering]

- The conventionally controversial axiom of choice follows from
  well-ordering, but of necessity it's now an axiom of
  finitely-specified choice.

- The conventional alternative to choice, namely the [axiom of
  determinacy](https://en.wikipedia.org/wiki/Axiom_of_determinacy),
  is moot.  It concerns perfect play in
  (certain) infinite games, but since it's not in general possible to
  compute the outcome of such a game even with a given pair of
  strategies, dicusssing existence of perfect strategies seems
  excessive.

[^computable-well-ordering]: This ordering is not, in general,
computable, because checking whether a given Turing machine is the
smallest that emits a given (infinite) object is not computable.  But
while I don't know offhand whether every computable set of computable
objects can be computably well-ordered, this is a technical rather
than philosophical question.

What of the real numbers?  There are only countably many Turing
machines, so the reals can't really exist in a universe of only
computable objects.[^formal-sets] What we have instead are the
[computable numbers](https://en.wikipedia.org/wiki/Computable_number),
which is the field of limits of
computably-[Cauchy](https://en.wikipedia.org/wiki/Cauchy_sequence)
computable sequences of rationals.  The famous [Cantor diagonalization
proof](https://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument)
that the classical reals are uncountable turns into a proof that there
is no _computable_ bijection between the computable numbers and the
integers.

[^formal-sets]: One can, of course, study sets as formal entities in
their own right---"the unique
[Dedekind-complete](https://en.wikipedia.org/wiki/Least-upper-bound_property)
totally-ordered field extension of the rationals" is itself a mental
object with reproducible properties, even if nearly all of the limit
points it consists of are not.  But how useful of a mental object is
it?

On the other hand, the unique characterization of the reals still
holds, and the computable numbers are indeed not complete.  Not only
is it not possible in general to compute the limit of a computable
sequence of rationals, there are even specific [computable sequences
whose limits are not computable
numbers](https://en.wikipedia.org/wiki/Specker_sequence).  (To get the
limit one also needs to compute the Cauchy condition, i.e., bound how
much farther the sequence may move.)  Never mind that if one tries to
make computable numbers totally ordered, one finds that equality is
not computable.

So the analysis one gets out of computable numbers is different from
real analysis.  Which analysis is better?  Computable analysis is more
faithful to reproducible reality: you really can't reliably compute
the limits of infinite sequences, and must instead ever watch for
accumulating errors.  On the other hand, real analysis is simpler:
even formulating the computable analogue of any nontrivial statement
requires thinking about what is to be computed, in what generality,
and given what inputs.  Real analysis is also better developed, having
predated attempts at computable analysis by centuries.

And so it is with infinity.  It's a simplification.  Only behvaiors
(of, e.g., computing processes) are infinite, but it's often
convenient to noun verbs and treat summaries of those behaviors as
objects in their own right.  But sometimes one needs information that
was summarized away, and the predictive power of the summary fails.

And that's the finitary finiversalist catechism:

- _What exists?_  Only what can be communicated about.

- _What can be communicated?_  Only finite descriptions.

- _What is infinite?_  Only behaviors of finite machines.

- _What are infinities?_  Only summaries of infinite behavior.

- _Do infinite or infinitesimal things exist?_  Only while they
  summarize behavior faithfully.

Notes
-----

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    Macros: {
      R: "{\\mathbb{R}}",
      Pr: "{\\mathrm{Pr}}",
      E: "\\mathbb{E}",
      eps: "\\varepsilon"
    }
  }
});
</script>
