---
title: Inference <em>by</em> Quadrature
date: 2017/12/25
author: Alexey Radul
published: true
---

Production-level probabilistic inference is usually said to be about
very high-dimensional problems.  The usual argument for the
techniques one learns (importance sampling, Markov chain Monte Carlo,
etc) starts from the curse of
dimensionality---that classic quadrature is hopelessly inefficient in
many (i.e., more than four) dimensions.  But what if one wants
probability in a low-dimensional problem? For example, one
might have a low-dimensional sub-problem of a more complex problem, READMORE or
one might want to test a scalable sampling technique on a low-d
version of some problem of interest.  How can quadrature be useful?

One way to use quadrature is to explicitly compute the normalization
constant of an unnormalized probability density.  Then, up to the
numerical error in the quadrature, one has the _normalized_ density
function, with which one can proceed to do whatever one wants.

Another way to use the same sort of grid of evaluations of the
un-normalized density is the technique called "Griddy Gibbs".
Griddy Gibbs is applicable when the density of interest is $p(x) = Z \tilde
p(x)$, where $Z$ is unknown but independent of $x$, and $\tilde p(x)$
is computationally tractable.  One needs, furthermore, that

- $X$ is one-dimensional;[^many-d]
- one wants to sample from an approximation to $p(x)$; and
- one can afford to evaluate $\tilde p(x)$ a fair number of times, say 100.

When might this happen?  The general lore is that if one's entire
problem were one-dimensional, one would not need samples.
Nonetheless, I can imagine at least two uses for high-quality
one-dimensional sampling methods: testing, and participating in a
larger Gibbs-style sampler.

[^many-d]: The idea generalizes to any dimensionality, of course.  It
is even plausibly useful in up to three or four, but I want to present
in 1-D for simplicity.

I have seen the latter arise in hierarchical Bayes quite often.
Generally, a hierarchical Bayesian model often factors as $p(D|\theta)
p(\theta|z) p(z)$, where $D$ are the data, $\theta$ are some latent
parameters of immediate interest (like the means of some clusters one
is trying to infer) and $z$ are some deeper latent parameters, like
the concentration parameter of one's prior on the number of clusters.
If, as happens all too often, there aren't enough conjugacies to
elimiate $z$ or $\theta$, one many choose a Gibbs-style sampler for
the overall problem, alternating sampling $\theta_{t+1} \sim
p(\theta|D, z_t)$ and $z_{t+1} \sim p(z|\theta_{t+1})$.[^m-h]
This kind of case fits Griddy Gibbs very well: I have
often seen the distribution $p(z|\theta)$ factor completely into
one-dimensional parts that can be sampled independently.  Furthermore,
$\theta$ is often much smaller than $D$, so the $\theta_{t+1} \sim
p(\theta|D, z_t)$ steps dominate the overall computation, and
therefore one can afford to evaluate $\tilde p(z|\theta)$ many times in each
$z_{t+1} \sim p(z|\theta_{t+1})$ step to get a good $z_{t+1}$.  Since
these models can be quite sensitive to the $z$ parameters, the
investment can pay off handsomely.  For example, every sampler I know
of for the [CrossCat](http://jmlr.org/papers/v17/11-392.html) model
uses some variant of Griddy Gibbs for the hyperparameters.

[^m-h]: Both samples may be approximate.  As long as the transition operators
$T_{z_t}(\theta_{t+1}|\theta_t)$ and $T_{\theta_{t+1}}(z_{t+1}|z_t)$
are ergodic and have the stationary distributions $p(\theta|D, z_t)$
and $p(z|\theta_{t+1})$, respectively, the overall chain will converge
to $p(\theta, z|D)$ as $t \to \infty$.

So, how does one actually do Griddy Gibbs?  The way I learned it was
this:

1. Evaluate $\tilde p(x)$ at some grid of points $x_i$.

2. Compute the normalization constant $\hat Z = \sum \tilde p(x_i)$.

3. Approximate $p(x)$ as a categorical distribution on that grid,
   $p(x) \approx \frac{1}{\hat Z} \sum \tilde p(x_i) \delta_{x_i}(x)$.

4. Sample from that.

This has always felt unsatisfying to me.  For instance, this
approxmation never admits an off-grid value for $x$.  Surely, I
thought, one can do better.  So I finally read the original Ritter and
Tanner 1991 techreport,[^open-access] and found that Mssrs Ritter and
Tanner indeed suggested doing better: they argue in the paper that any
quadrature technique of one's choosing can form a legitimate
approximation to $p(x)$ from a grid of evaluations of $\tilde p(x)$,
wherefrom one can then sample.

[^open-access]: The published version, Ritter and Tanner 1992,
does not appear to be open access.

Here's one such algorithm, with a piecewise linear
rather than piecewise constant approximation to the CDF of $p(x)$:

1. Evaluate $\tilde p(x)$ at some ordered grid of points $x_0 < x_1 < \ldots < x_i < \ldots < x_n$.

2. Compute $\hat Z = \sum_{i=0}^{n-1} \tilde p(x_i) (x_{i+1} - x_i)$.

3. Approximate $p(x)$ as the piecewise constant PDF
   $$ p(x) \approx \hat p(x) = \begin{cases} 0 & x < x_0 \\ \frac{\tilde p(x_i)}{Z} & x_i \leq x < x_{i+1} \\ 0 & x \geq x_n \end{cases} $$
   (whose CDF is therefore piecewise linear).

4. Sample from that.

This looks a lot better.  One reason I suspect it may not have caught
on as much is that there are several free choices embedded in what I
presented, leaving more work for the practitioner who wishes to adopt
it.  For instance, instead of using the value of $\tilde p$ at the
lower end point of the interval $(x_i, x_{i+1})$, one can use the
average: $\hat p(x) = (\tilde p(x_i) + \tilde p(x_{i+1})) / 2 \hat Z'$
(which $\hat Z'$ needs to be computed accordingly, and is not equal to
$\hat Z$).  Also, instead of setting the mass outside $(x_0, x_n)$ to
$0$ as I have done, one can posit some tails of some functional form
with total mass $\varepsilon$ (which becomes a parameter of the
method) and scale the rest of the PDF accordingly.  One can alternately deal
with infinite support by using some change of variable formula to do
the quadrature over a finite interval.  In the context of defining a
general library or programming language for inference, I think the
gain is worth the cost, but I can see why a practitioner writing a
one-off inference scheme may decide to avoid this choice space.

I want to point out one more possible adjustment.[^mine]  As I learned Griddy
Gibbs, one performs inference by sampling directly from the computed
$\hat p(x)$, accepting the difference between it and the target $p(x)$
as error in one's ultimate solution.  However, if one is trying to set
up a convergent Markov chain, one can also treat $\hat p(x)$ as a
Metropolis-Hastings proposal distribution, and guarantee convergence
to the right target by accepting or rejecting it.  Such a step looks
like

[^mine]: I have not seen this refinement in the literature, but that's probably
because I didn't look hard enough.

1. As above.

2. As above.

3. As above.

4. Propose to move to $x_{t+1} \sim \hat p(x)$.

5. Compute the acceptance ratio[^independence]
   $$ \alpha = \frac{\tilde p(x_{t+1}) \hat p(x_t)}{\tilde p(x_t) \hat p(x_{t+1})}. $$

6. Move to $x_{t+1}$ with probability $\max(1, \alpha)$, otherwise stay at $x_t$.

Here we have gained (as one always does with Metropolis-Hastings)
accuracy in the limiting distribution in exchange for the speed of
reaching it.  Indeed, if the approximation $\hat p(x)$ is good
everywhere, then each term $\hat p(x_t) / \tilde p(x_t)$ will be close
to $1$, and this chain will almost always accept.  However, if the
approximation is off somewhere, that will show up as rejections.

[^independence]: As written, $\alpha$ is correct if the choice of grid
$x_i$ does not depend on the current state $x_t$.  If $x_i$ does
depend on $x_t$, computing the reverse probability calls for repeating
the quadrature on the grid points $x_{t+1}$ determines.  This is fine---one gives up 2x
in affordable grid resolution in exchange for adapting the grid to the areas the
chain wants to explore.  For instance, doing a pair of
[tanh-sinh](https://en.wikipedia.org/wiki/Tanh-sinh_quadrature)
integrations over $(-\infty, x_t)$ and $(x_t, \infty)$ would be a way to
focus a lot of computational attention near the current point, which should
be good for exploring a tall peak (once the chain found it).

As a further refinement, a practitioner could monitor the rejection
rate in order to assess how good of an approximation they got out of
their quadrature technique.  I can even imagine automatically adapting
the fineness of the grid, say, to maintain a target acceptance rate as
a chain proceeds.

A benefit, as far as I can tell, of embedding Griddy Gibbs in
Metropolis-Hastings is that the usual problems of trying to develop a
generic numerical quadrature method get masked by the accept/reject
step.  For instance, suppose $\tilde p(x)$ has a pole inside the
domain of integration.  Then any approximation $\hat p(x)$ must cut
off substantial mass, due to finite resolution near the pole.[^pole]
Evaluated as quadrature, this leads to potentially unacceptable error
in the answers.  But evaluated as a proposal distribution, this just
leads to a slowdown in convergence---the chain will fix the
underapproximation of the pole by tending to reject moves away from
it.  The same thing happens if the approximation's tails are too thin
(as long as they are non-zero).  The accept/reject step provides
an automatic mis-fit detection mechiansm.

[^pole]: Unless, of course, the operator knows where the pole is
and deploys appropriate tactics to tame it, but that can only
work on a case-by-case basis.

In summary, I think quadrature is a very useful tool in the
inferential kit, and I'm sad that I didn't get a chance to [implement
it in Venture](https://github.com/probcomp/Venturecxx/issues/642).

References
----------

- Ritter, C. & Tanner, M., "The Griddy Gibbs sampler", 1991,
  Technical Report #878, Department of Statistics, University of Wisconsin-Madison
  [https://www.stat.wisc.edu/sites/default/files/tr878_0.pdf](https://www.stat.wisc.edu/sites/default/files/tr878_0.pdf)

- Ritter, C. & Tanner, M., "Facilitating the Gibbs Sampler: The Gibbs
  Stopper and the Griddy-Gibbs Sampler", 1992, Journal of the American
  Statistical Association (87) pp. 861-868

Notes
-----

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    equationNumbers: { autoNumber: "AMS" },
    noErrors: { disabled: true },
  }
});
</script>
