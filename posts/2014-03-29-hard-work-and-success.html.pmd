---
title: Hard Work <em>and</em> Success
date: 2014/03/29
author: Alexey Radul
---

This essay is inspired by the assertion that hard work is more
important for success than being smart.  There are, of course, many
meta-objections one could have against such an assertion, READMORE especially
in the context in which I heard it.  For example, one could notice that
the speaker, being American, might be culturally obligated to say
something like this.  Or one could observe that speaking to an audience
of people who wish to be told how to behave in order to be successful
forces suggesting something the audience will think they can actually
do.  Or one could reflect on who benefits from a general milieu that
encourages working hard.  I am not concerned with these meta-objections, though.

In support of this assertion, the particular speaker adduced the empirical
observation that most successful people work hard rather than are smart;
and also that plenty of smart people do not work hard and are not
successful.  This sounds compelling on the surface, but is still open
to objection.  First, of course, the data being from a survey, the
results will be much biased by whether successful people feel
obligated to downplay their intelligence (and privilege) in favor of
the more culturally acceptable hard work; but there is also the issue
that even if all information transmission mechanisms are perfect,
looking only at successful people cannot provide adequate information
about the factors that matter to success, because it neglects to
account for the prevalence of those factors in failure.  This essay
consists of a worked example illustrating this point.

Let us take a somewhat fanciful little model, and
see what features it has.  Suppose success is driven by both
talent and hard work, and also chance, in the following way: Let all
individuals be born with some normally-distributed level of talent.
Suppose that there is some threshold of talent, say 3 standard
deviations out, that is required for success---that is, people whose
talent exceeds the mean by 3$\sigma$ or more may be successful, and
others will not.  Suppose society calls these people "smart".  But let
there be two wrinkles: suppose also that any individual can choose to
work hard, which imposes some cost but increases their effective
talent by one $\sigma$; and suppose furthermore that among all the people
whose effective talent exceeds 3$\sigma$, 10% succeed at random and the
rest fail.  In equations:

$$\begin{eqnarray*}
  talent & = & N(0, \sigma)\\
  talent_{eff} & = & 
    \begin{cases}
      talent &           \mbox{if slacking off}\\
      talent + \sigma &  \mbox{if working hard} \end{cases} \\
  P(\mbox{success}) & = &
    \begin{cases}
      0.1 &  \mbox{if } talent_{eff} > 3\sigma\\
      0   &  \mbox{otherwise}\end{cases}\\
\end{eqnarray*}$$

If we furthermore suppose that i) everyone wants to succeed, ii) no
one wants to work hard if it will have no effect on their success, and
iii) everyone knows their endowment (and all the rules), then the
following things will happen:

- 0.13% of people will be called "smart" (talent > 3$\sigma$), but
  none of them will work hard (why bother?)
- 2.15% of people will have 2$\sigma$ < talent < 3$\sigma$,
  so will work hard and be eligible to succeed.
- The remaining 97.72% of people will not work hard and will not
  succeed.

After the randomness of success is taken into account, the
distribution of results will look like this (as percentages of the
overall population):

: Outcomes for our hypothetical population

+--------------------------------+-----------+---------+
|                                | Succeeded |  Failed |
+================================+===========+=========+
| Smart and worked hard          |    0      |  0      |
+--------------------------------+-----------+---------+
| Smart and didn't work hard     |    0.013% |  0.117% |
+--------------------------------+-----------+---------+
| Not smart and worked hard      |    0.215% |  1.935% |
+--------------------------------+-----------+---------+
| Not smart and didn't work hard |    0      | 97.720% |
+--------------------------------+-----------+---------+
| Total                          |    0.228% | 99.772% |
+--------------------------------+-----------+---------+

But we are tempted to examine the data selectively.  If we
look just at people who succeeded, it looks like this:

: Concomitants of success in our model

+---------------------------+-----------+
|                           | Succeeded |
+===========================+===========+
| Smart                     |   5.7%    |
+---------------------------+-----------+
| Not smart but worked hard |   94.3%   |
+---------------------------+-----------+
| Total                     |   100%    |
+---------------------------+-----------+

That's a pretty compelling-looking case for hard work.  What if we do
some homework and include people who obviously could have succeeded
but didn't (that is, those who were smart to begin with)?

: Concomitants of potential to succeed in our model

+---------------------------+-----------+---------+
|                           | Succeeded |  Failed |
+===========================+===========+=========+
| Smart                     |   3.8%    |   33.9% |
+---------------------------+-----------+---------+
| Not smart but worked hard |  62.3%    | no data |
+---------------------------+-----------+---------+
| Total                     |  66.1%    |   33.9% |
+---------------------------+-----------+---------+

If anything, that makes the case for hard work seem even stronger.  But
what's the real story?  If you weren't born into this world yet, would
you rather be born smart or commit to working hard? [^fine-point]

[^fine-point]: Note that there
is a fine point in the last question: in this model, the only people
who actually do work hard are the ones for whom it makes a difference,
so working hard is not independent of being at least a little smart.
But we need to artificially separate these two variables to judge the
value of hard work as such, so we need to assume a hypothetical person
who always works hard regardless of their endowment.

$$\begin{eqnarray*}
  P(\mbox{success}|\mbox{smart}) & = & P(\mbox{success}|talent_{eff} > 3\sigma) \\
  & = & 0.1 = 10\% \\
  P(\mbox{success}|\mbox{work}) & = &
    P(talent > 2\sigma) \cdot P(\mbox{success}|talent_{eff} > 3\sigma) \\
  & = & .0228 \cdot 0.1 = 0.228\%
\end{eqnarray*}$$

I know which of those chances I would rather take. [^advice] [^luck]

How did our examination of the data lead to such poor intuitions about
what actually happens in this world?  Because those examinations were
constrained by a pretty harsh filter: looking at only the successful,
or only those who were smart or successful, shows us only 0.228% or
0.345% of this population, respectively.  Since the factors we are trying to
study affect individuals' membership in the group we observe, the selection
grossly distorts our statistics.

This is not to say that I am encouraging my dear readers to slack
off---on the contrary, working hard at the proper kind of work can be
<a href="http://www.slate.com/articles/technology/technology/2014/01/do_what_you_love_love_what_you_do_an_omnipresent_mantra_that_s_bad_for_work.html">its
own reward</a>.  This is just a cautionary tale about statistical
arguments, and when one ought to find them convincing.

[^advice]: Note that even though in the abstract, smarts are much more
important to success in this model than hard work, hard work is still the
only thing an individual can affect, so "work hard" is still good
advice---only the justification "it matters more than talent" is
wrong.

    Actually, whether it's good advice or not can be a mathematical
question too, if we specify how obnoxious hard work is.  If I am
giving advice to a completely random individual from this world, then
as far as I know, P(2$\sigma$ < their talent < 3$\sigma$) is 2.15%; so if
succeeding is at least 500 times better than working hard is bad, my
expectation of their well-being increases if they decide to work hard.
That ratio goes down significantly if we consider that maybe they
wouldn't have asked me for advice if they didn't sense that they at
least had some chance of becoming successful; etc.

[^luck]: What about being born lucky?  That is, imagine an individual
to whom success will come automatically if they manage to get their
effective talent above 3$\sigma$.  For them, if they work hard when
appropriate instead of leaning exclusively on their luck,

    $$ P(\mbox{success}|\mbox{luck}) = P(talent > 2\sigma) = 2.28\% $$
so in this particular world, it's better to be smart than lucky too.
But that's very dependent on the choice of parameter values.

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
