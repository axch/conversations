---
title: <em>How to</em> Take CSS <em>to the</em> Next Level
date: 2013/10/19 19:27:00
author: Alexey Radul
---

Computationally generating the semantics of a web page---the
HTML---has become the standard modus operandi on the web.  No serious
web developer writes by hand the HTML that their server sends to the
browser anymore; invariably, some template engine or other generator
intermediates, removing tedium and adding flexibility and power.  The
same thing is slowly happening to the presentation layer as well---the
CSS.  This article discusses the high technology that can be brought
to bear on computed web layout and styling, both as an advertisement
for some impressive existing tools and as a call to action for
toolsmiths to fill the gaps I observe.  READMORE

Contents
--------

- [Compute with Variables and Functions](#compute-with-variables-and-functions)
- [Read the DOM](#read-the-dom)
    - [A JavaScript Runtime can do Anything](#a-javascript-runtime-can-do-anything)
    - [Take Functional Reactive Programming to the Web](#take-functional-reactive-programming-to-the-web)
    - [Compile Magic Variables to Media Queries](#compile-magic-variables-to-media-queries)
    - [Compile More Magic to Many Queries](#compile-more-magic-to-many-queries)
- [Solve Linear Constraints](#solve-linear-constraints)
    - [A JavaScript Runtime Can Do That, Too](#a-javascript-runtime-can-do-that-too)
    - [Intermezzo: Constraint Programs Always Guess Right](#intermezzo-constraint-programs-always-guessright)
    - [Solve Static Constraints at Compile Time](#solve-static-constraints-at-compile-time)
- [Solve Discrete Constraints, Too](#solve-discrete-constraints-too)
- [Explain Solutions of Constraint Systems](#explain-solutions-of-constraint-systems)
- [Take CSS to the Next Level](#take-css-to-the-next-level)

Compute _with_ Variables _and_ Functions
----------------------------------------

Variables and functions are essential tools for writing anything in
any language, and it is high time CSS acquired them.  The world has
known how to implement subroutines for over fifty years, and lexical
scope for nearly forty, so it is with great pleasure that I refer the
gentle reader to [SASS](http://sass-lang.com/) and
[Stylus](https://learnboost.github.io/stylus/).  Each is a polished,
productized tool that finally makes CSS an actual language, and all
you have to do is insert a code generator into your workflow.

I haven't actually used SASS or Stylus much myself, but I've looked
over the documentation, and I am impressed with how careful they both
are to just drop in to a develper's existing workflow.  Standard CSS
is valid input for either of these tools and compiles to itself.
Additional constructs like variables, functions, arithemtic, and so on
are all resolved completely during compilation, producing normal,
standards-compliant CSS.  Even your web server doesn't get any
additional load from your using one of these tools, and the user's
browser is certainly none the wiser.

I salute the work that Stylus and SASS represent and the benefits they
bring.  I have, however, nothing to say about the technology that
makes them possible---as far as I can tell, everything there is pretty
standard, well-tested programming language stuff.  Buckle your
seatbelt: the ride is about to get wilder.

Read _the_ DOM
--------------

Every web developer is painfully aware of the problems caused by their
audiences viewing their web pages on screens of wildly different size
and resolution, and now even different precision of interaction (mice
are way easier to aim with than touch screens).  Every web developer
is also painfully aware of the impracticality of developing completely
separate versions of a web page for each possible device---it's tons
of work, and keeping them in sync is a nightmare.

Responsive web design is a philosophy of trying to build single web
designs that, by incorporating appropriate points of flexibility,
automatically and gracefully adapt to being viewed on various devices.
If styling is a real language, why not introduce magic variables whose
values depend on the user's context?  It would make responsive design
so much easier!

### _A_ JavaScript Runtime _can do_ Anything

If one goes the JavaScript route, it becomes possible for responsive
design to respond not just to gross features of the user's device,
such as the width of their screen, but to subtle consequences of the
user's choices.  For example, suppose you want to adjust the style of
some piece of text depending on whether it fits on at most two lines:

```css
span#do-not-be-too-tall {
  /* Measure the element's height and line-height with -> syntax */
  if ->height >= 3 * ->line-height
    font-size: 90%; /* and try to fit in two lines */
}
```

That depends not only on the width of the screen, but also on the font
size (which the user can change), the actual font (which will depend
on the fonts available on the user's system), and the decisions made
by the browser's hyphenation, justification, and line breaking
algorithms.  No pure-CSS design can be that responsive---there aren't
enough media queries for your CSS to be able to discern all these
relevant variables, even if you could simulate the browser's rendering
engine well enough to decide at compile time what to do in each
configuration.

Such responsiveness can, however, be implemented with a JavaScript
runtime system that, on window load and other relevant events, examines the
rendered metrics of appropriate DOM elements and adds or removes
appropriate styles in response.  There are nice efficiency problems
here, for intance to decide which properties of which elements need to be
inspected under which circumstances, and which computations ought to
be redone in order to decide which properties to adjust.

### _Take_ Functional Reactive _Programming to the_ Web

One direction in which the JavaScript path can lead is full-blown [functional
reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming)
on the web.  In brief, functional reactive programming (FRP) starts
with viewing user inputs as either time-varying signals (like the
position of the mouse) or chronological event streams (like mouse
clicks).  The big idea is that functions written as though they
operate at a single instant in time can be lifted to operate sensibly
on these signals, and the job of updating state (which would normally
end up being a mess of callbacks) can be handled smoothly and
automatically by the runtime system.  A [classic
example](http://www.flapjax-lang.org/demos/index.html#follow) would be
writing code that looks like

```css
#mouse-follower {
  position: absolute;
  left: %mouse%->left;
  top: %mouse%->top;
}
```

and having that compile to some JavaScript that causes the item with
id `mouse-follower` to follow the mouse around on the screen---without
the developer writing any callbacks or mouse event handlers.

I know about two tools that implement FRP for the web, which I highly
recommend you check out: [Flapjax](http://www.flapjax-lang.org/) and
[Elm](http://elm-lang.org/).  I haven't had a chance to use them
myself, but the backing technology is really cool.  On the other hand,
I expect them not to be as polished as a SASS or a Stylus---functional
reactive programming is a more recent academic development than
something as basic as subroutines, and I know that Flapjax at least
was born as a research project rather than explicitly a product
(though it may have evolved since).

In a sense, FRP-on-the-web subsumes reactive web design.  A toolkit that
makes it really easy to write rich user interfaces that smoothly
update themselves in response to mouse movements, keystrokes,
gestures, and clicks should be great for the task of writing user
interfaces that smoothly update themselves when the user changes the
width of their screen or the size of their font---just make those
things be variables also, instead of hardcoding your expectations for
them.

<a id="overkill"></a>In a different sense, the needs and constraints
of reactive web design may make web-FRP overkill.  FRP worries about
high-frequency events like the whereabouts of the user's mouse, which
require all sorts of fancy technology to react to efficiently, both at
compile time and at runtime (no way to do FRP on the web without
JavaScript, and plenty of it!).  Reactive web design, on the other
hand, is interested in things that change rarely or even not at all
during any given page view---screen width, font size, pointer
precision---but expects extreme performance and portability.  Being
able to run on lots of devices is the point, so why rule out devices
that can't or won't execute JavaScript fast enough, or even at all?

### _Compile_ Magic Variables _to_ Media Queries

It seems to me like there is a technology gap between Stylus, which
emits pure CSS but offers no support for adjusting the presentation
based on the user's context, and Flapjax, which can react to anything
at all, but needs a JavaScript runtime system to even work.

Perhaps one could augment Stylus (or another tool in the
same niche) to permit a limited set of variables whose values are
semantically determined by the user's browser.  If such variables only
affect the actual layout through conditionals, then it should be
possible to compile them to media queries.  For example, a variable
called `%width%` could, semantically, always be set to the width of
the window viewing the page.  Then the developer can write something
like

```css
#sidebar {
  width: 100px;
  if %width% < 100px
    display: none;
}
```

to suppress a 100px wide element if the user's window is too narrow
for it.  The media query implementation of that segment could look
like

```css
#sidebar {
  width: 100px;
}

@media all and (max-width:100px) {
  #sidebar {
    display: none;
  }
}
```

Note that even this feature needs more than a completely brain-dead syntax translation
because the media query appears outside the selector, and part of the
style does not depend on the media query.  Of course, proper
variables should also be able to participate in functions and arithmetic
and such, so the following ought to work for disappearing an element
that should be 100px wide but should occupy no more than half the
screen:

```css
sidebar_width = 100px

#sidebar {
  width: sidebar_width
  if %width%/2 < sidebar_width
    display: none;
}
```

Such a thing ought to turn into

```css
#sidebar {
  width: 100px;
}

/* System needs to find the inequality %width%/2 < 100px
   and solve it for %width% */
@media all and (max-width:200px) {
  #sidebar {
    display: none;
  }
}
```

<a id="symbolic"></a>Implementing such magic variables properly seems
to me to call first for [symbolically evaluating](https://en.wikipedia.org/wiki/Symbolic_execution)
the stylesheet with respect to unknown values of the magic variables
(in order to detect the conditionals they participate in), and second
for solving the inequalities those conditionals represent to discover
the points where the stylesheet's behavior changes.  Those points can
then become the values that the media queries compare against.  I
imagine that constant-coefficient linear inequalities ought to be
enough for the needs of web UIs, so this is eminently doable.

### _Compile_ More Magic _to_ Many Queries

A more sophisticated thing would be to allow the magic variables to
influence the actual values in the stylesheet.  For example,

```css
#screen {
  width: %width%;
  height: 9*%width%/16;
}
```

would specify a full-width viewport with a 9x16 aspect ratio; which I
do not think is possible with standard CSS, because a height cannot be
specified as a percentage of a width.[^css-functions]

I doubt such a feature as this can be implemented perfectly with media
queries or any other pure-CSS mechanism, but a jaggy version could be
done with no JavaScript.  To do it, compute (at compile time) what the style
should be for every value of `%width%` between 0 and 3000px in 20px
increments, and use media queries to select the one of those that best
matches the user's window.  Choosing those constants ought to be
manual, because it exposes a trade-off between resolution (how close
to the user's actual window width the layout gets, and how jumpy the
transition is as the window is resized) and the size and rendering
speed of the resulting stylesheet.  It would be appropriate to use
something like symbolic evaluation to detect which variables do not,
in fact, affect the layout in a continuous way, so that effort is not
wasted on sampling them like this.

Solve _Linear_ Constraints
--------------------------

The things that CSS allows a designer to control conveniently aren't
actually the things that are important about a layout.  It doesn't
really matter that some `div` is 397px wide; what matters is that the
layout's two columns should be the same width, and they should fit
into their container side-by-side, with a gap between them.

A general way to say that is that we want the elements of our
presentation to stand in some relationships with each other (such as
being the same width); and much of the thankless part of web design is about
achieving those relationships with the available tools (such as
specifying exact widths for things).  Wouldn't it be better to just
spell out the desired relationships, and let the computer figure out
how to arrange things to satisfy them?

This phenomenon is not specific to the web, and there has been a
steady trickle of research on so-called constraint-based interfaces
over the years.  One system of note is
[Cassowary](https://constraints.cs.washington.edu/cassowary/),
a [linear program](https://en.wikipedia.org/wiki/Linear_programming)
solver designed with the needs of user interfaces in mind.
Specifically, the solver is incremental, in the sense that it has
algorithms for very quickly updating the solution to an already-solved
a constraint system under small changes (including adding and removing
constraints).  Such re-solving corresponds to updating the UI in response to user actions.
The solver also supports constraints of varying priorities, where the
low-priority constraints get violated if the system as a whole is
otherwise unsatisfiable.  Graceful degradation like that is critical for smoothly handling
unexpected or extreme situations.

Constraint-based interfaces seem like a really good idea to me, though
as far as I know, they have yet to really catch on.  Maybe the block
was that putting even a little constraint into your design used to
require shifting your whole system to use one of the constraint-based
UI packages, I don't know.  Apple [seems to be incorporating this
technology](https://developer.apple.com/library/mac/documentation/UserExperience/Conceptual/AutolayoutPG/Introduction/Introduction.html),
though.

### A JavaScript Runtime can do That, Too

What might [constraint-based layout with
CSS](https://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4819)
look like?  There would of course need to be some syntax for
specifying constraints and preferences; the example of two columns and
a separator might look like this:

```
require(#left->width = #right->width)
require(#left->width + #right->width + #gap->width < #container->width)
maximize(#left->width)
```

And then there would need to be a constraint solver such as Cassowary
somewhere that computes solutions.

In fact, there are projects focused on [reimplementing Cassowary in
JavaScript](https://github.com/slightlyoff/cassowary.js) and [using it
for web layout](http://gridstylesheets.org/).  Permit me to augment my
earlier recommendation: Check out [Grid Style
Sheets](http://gridstylesheets.org/) too.  It's a compiler that
transforms constraint-CSS to instructions for a JavaScript runtime
system to measure the relevant aspects of the DOM and solve the
constraint system.  The runtime uses Cassowary's algorithms to
incrementally update the solution as DOM variables change.  As of this
writing, it looks like a very new project, so you would get "all the
rights and privileges" of being an early adopter.

Note that the expressive power of Flapjax (and Elm) is probably not
comparable to that of GSS.  GSS allows the UI to be specified with
declarative, potentially cyclic and over-specified constraints that
are automatically solved, whereas Flapjax and Elm require the developer to
decide on the order in which values are computed and write down
functions defining each variable in terms of earlier ones.  On the
other hand, Flapjax and Elm can handle variables of all types, including
discrete values and event streams, whereas the Cassowary algorithms
that GSS is based on only solve linear programs, so cannot operate on
discrete variables.  This divergence is not an accident: solving acyclic systems
like Flapjax or linear systems like Cassowary is asymptotically fast,
whereas cyclic systems of discrete equations are in general
[NP-hard](https://en.wikipedia.org/wiki/P_versus_NP_problem).  I will [get back to
this](#solve-discrete-constraints-too).

Aside: I imagine that Grid Style Sheets does not do either of the
following optimizations, so there may be a good opportunity to improve
its performance, perhaps considerably.  First, the browsers' existing
layout engines already solve certain kinds of constraints natively.
For example, `position: absolute; top: 5px`
constrains a UI element relative to its enclosing element.
Presumably everything would be more efficient if the compiled
output represented whatever constraints it could in terms of ones the
browsers can solve natively, and relied on JavaScript only for the
unavoidable cases.

Second, given a fixed DOM and stylesheet, it should be possible to
[partial-evaluate](https://en.wikipedia.org/wiki/Partial_evaluation)
the JavaScript constraint solving engine with respect
to the actual constraints it is to solve.  If this works, the residual
ought to be pretty efficient (especially since the optimization loop should
be implementable as an [asm.js](http://asmjs.org/) module).  Then
again, this may not be so easy, because JavaScript elsewhere on the
page may do things like add or remove elements that match the given
selectors, changing the constraint set that needs to be solved.

### _Intermezzo:_ Constraint Programs _always_ Guess&nbsp;Right

The presence of constraint solving machinery enables a very powerful
and liberating programming style.  A tiny amount of linguistic support
suffices to allow the developer to call forth unknown quantities (say,
lengths) whose values will be determined by solving the constraints:

```css
mylen = a-length()
#foo {
  ... use mylen
}
#bar {
  ... use mylen some more
}
... require something that constrains mylen
```

With this feature, coding feels like you can ask your program to
guess, and depend upon it always guessing right (to wit, in a way that
satisfies the constraints, which can appear later).

<blockquote class="pullquote-display"><p>
Standard programming requires the developer to diagonalize the
system of constraints in their head before writing the program.
</p></blockquote>

Guessing and constraining is important as a programming style in part because
it allows a new form of modularity: the unkown values can be created
and used in one part of a program (module, if you will) and
constrained in another.  In other words, some of the relationships
that some variable participates in can be spelled out before the value
of that variable could possibly be known.  Standard (read: imperative,
functional, or object-oriented) programming styles, in contrast,
essentially require the developer to diagonalize the system of
constraints in their head before writing the program, so as to be able
to write code that computes each value before it is used.  This is bad
not only because it is work, but because such diagonalization impedes
modularity by having to span what could otherwise be module
boundaries.

### _Solve_ Static _Constraints at_ Compile Time 

Grid Style Sheets shares with Flapjax and Elm the potentially
undesirable feature of requiring a JavaScript runtime system to work.
As discussed [above](#overkill), this imposes weight---on the
developer's toolchain, on portability, and on the user's ultimate
experience with the page.

There is, however, no reason to solve all the constraints
that may appear in a stylesheet at runtime.  If we return to
our example with the two columns,

```
require(#left->width = #right->width)
require(#left->width + #right->width + #gap->width < #container->width)
maximize(#left->width)
```

this system could be solvable completely statically if the gap width
and container width are specified elsewhere in the stylesheet.  In
this case, a CSS preprocessor occupying the Stylus niche could just
solve that constraint system at compile time, and emit pure,
JavaScript-free CSS with the solution embedded in it.  It could even
be done with an off-the-shelf external linear program solver (like
Cassowary!) running on the developer's desktop.

Some degree of responsive design could be injected into this with the
[many queries trick](#compile-more-magic-to-many-queries): if you have
a constraint system whose solution depends on the width of the user's
window but not on anything else about the user's environment, you
could solve it at compile time for each possible value of that width
(up to some resolution), and emit media queries that choose among
stylesheets embodying the various solutions depending on which point
the true value is closest to.  An incremental linear program solver
would offer a substantial benefit to compilation times here.

_Solve_ Discrete Constraints, _Too_
-----------------------------------

My feeling is that linear constraint satisfaction is great, but for
serious styling and layout design, developers really want constraints
on discrete variables too.  With such technology, making a navigation
bar appear across the top or down the side depending on where it fits
could look like this:

```css
#navbar {
  orientation = a-value(above, beside);
  if orientation = above
    ...
  if orientation = beside
    ...
}
... require the navbar to fit ...
/* might be automatic with min-width properties and such */
```

Solving systems of guesses like this translates into an instance of
the [boolean satisfiability
problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem).
If such discrete variables interact seriously enough with the linear
variables (for instance, turning linear constraints on or off, or
doing something depending on some inequality over linearly-constrained
variables), then it's a case of [mixed integer-linear
programming](https://en.wikipedia.org/wiki/Integer_programming).

Even though in general both of these problems are NP-hard, in practice
I cannot imagine any sane layout having enough embedded guesses to
stymie any self-respecting SAT or MILP algorithm.  An adequate
algorithm for batch-mode solution of one layout-sized system of linear
and discrete guesses would not be at all difficult to implement.

If the constraints on the guesses do not reference any client-side
parameters (in any way that may affect the solution), the compiler
should just solve the system (maybe with an off-the-shelf solver,
even) and emit CSS specialized to the answers.  If the validity of the
guesses depends on client-side values only in a discrete way, it may
be feasible to compute all the relevant regions of parameter space by
a [symbolic evaluation mechanism](#symbolic) and emit CSS that checks
what region it's in using media queries.  For systems where that
wouldn't work, but for only a few client-side parameters, it may also
be possible to use the [many queries
trick](#compile-more-magic-to-many-queries) to produce jaggy but
JavaScript-free CSS output.

It is not clear to me that the other implementation strategy, namely
putting a full mixed integer-linear program solver into a JavaScript
runtime system, would be a very good idea.  There are no fast
algorithms for solving MILPs.  Worse, I don't know of any incremental
algorithms, namely ones that can quickly adapt the solution of one
MILP to solve a similar one; and re-solving the whole layout every
time anything changes could be prohibitive.

Then again, it could also be just fine.  Flapjax, Elm, and Cassowary
uniformly do not do this partly because they were all intended to
support very interactive user interfaces, where computing the layout
had to keep up with the user typing and moving their mouse.  A tool
whose goal is just responsive web design and constraint-based layout
may well afford to solve a modest mixed integer-linear program once on
page load plus every time the width of the user's window changes.

Explain Solutions _of Constraint Systems_
-----------------------------------------

With power comes responsiblity.  A constraint-based version of CSS
such as I have suggested brings the possibility for a whole new kind
of bug: systems of constraints that are accidentally unsolvable, or
ones that cause the compiler or the runtime to choose a solution that
was not what the developer intended.  Debugging such sitations, even
with just a couple dozen constraints, could easily turn into a
nightmare.

Therefore, I think it would be quite important for any CSS tool
along these lines to provide debugging support, specifically in the form of
explanations.  There should be some appropriate interface
(perhaps accessible only in code generated with a "debug" flag or
something) that lets the developer select some element and ask "why is
this here?"; or indicate some variable and ask "why does this have
this value?".  The answer to this kind of question consists of the
relevant constraint(s) that forced the variable in question to its
value.  Often, constraints become forcing only because of the values
of other variables, so a recursive explanation may be necessary.  Such
explanations also apply to the question "why is this constraint not
satisfiable?"

<blockquote class="pullquote-display"><p>
Implementing linear or integer solvers that actually pay
attention to the sparse structure of the problem and use it to explain
the solution would be both nontrivial and an advance.
</p></blockquote>

Making such explanations would actually be some amount of work.
Formalisms like linear programming and integer programming culturally
assume the problem is dense: there are variables, there are
constraints, and both the formalisms and the usual algorithms assume
that every variable might as well appear in every constraint.  In
practice, though, actual problems tend to be pretty sparse: usually
any given constraint will only touch a handful of variables, and any
given variable will only participate in a handful of constraints.  So
the question "why did this variable get this value" actually has a
richly structured answer: only a handful of constraints were
immediately relevant, and an only somewhat larger handful of variables
were relevant to those constraints, etc.[^disconnected]

It may be possible to recover the sparse structure of the problem by
augmenting the solution algorithm to record, in some appropriate
datastructure, the pieces of information each of its solution actions
depended upon.  Then, reading the relevant portion of such a
dependency trace backwards may constitute a reasonable explanation for
a given part of a solution.  Then again, it may even be possible to
construct explanations just by examining the solution and the
problem---the value of `x` in the optimal solution is this because
trying to increase `x` would voliate constraint foo, given the values
of `y` and `z`; the value of `y` in the optimal solution is that
because, etc.  It may also turn out that compelling explanations
require a mixture of both techniques, or that one kind of technique
works well for discrete systems and another for linear ones.  In any
case, implementing linear or integer solvers that actually pay
attention to the sparse structure of the problem and use it to explain
the solution would be both nontrivial and an advance.

Take CSS _to the_ Next Level
----------------------------

If you are a practicing web developer, you can take your own web
designs to the next level by trying one of the tools I have been
talking about.  In the case of Flapjax, Elm, or GSS, it may take some
time to get used to the design possibilities they offer, but once your
mind has been stretched by that new idea, I do not expect it to ever
regain its original dimensions.

As a recap, here is the space of tools as I see it:

+-------------------------+---------+---------+---------+
| Feature                 | Stylus  | Flapjax | GSS     |
+=========================+=========+=========+=========+
| JS-free usage           | **yes** | no      | no      |
+-------------------------+---------+---------+---------+
| Continuous Computations | **yes** | **yes** | **yes** |
+-------------------------+---------+---------+---------+
| Discrete Computations   | **yes** | **yes** | no      |
+-------------------------+---------+---------+---------+
| User Functions          | **yes** | **yes** | no?     |
+-------------------------+---------+---------+---------+
| Responsiveness          | no      | **yes** | **yes** |
+-------------------------+---------+---------+---------+
| Reactivity              | no      | **yes** | no?     |
+-------------------------+---------+---------+---------+
| Linear Constraints      | no      | no      | **yes** |
+-------------------------+---------+---------+---------+
| Discrete Constraints    | no      | no      | no      |
+-------------------------+---------+---------+---------+
| Explanations of Results | no      | no      | no      |
+-------------------------+---------+---------+---------+

: Feature space of existing next-level CSS tools.

The Stylus column goes for SASS too.  The Flapjax column goes for Elm
too.  Question marks mean that I am not sure because the documentation
isn't clear enough to definitely rule a capability out.
"Responsiveness" means taking into account low-frequency client-side
values like the user's window width, whereas "reactivity" means also
taking into account high-frequency ones like mouse position.

If you are into making tools as well as web design, one way you can
help take everyone else's CSS to the next level is to take one of
these thoughts and implement it, either in an appropriate existing
tool, or, if that doesn't fit, a new one.  As a recap, here's the
project list as I see it:

- Augment the compiler in any tool to **symbolically evaluate** style
  rules with respect to media-queryable variables.  From this, detect
  regions where behavior changes, and extract the boundaries of those
  regions to media queries.

    - For SASS or Stylus this would provide support for responsive web
      design.

    - For Flapjax, Elm, or GSS, this could improve performance, by
      offloading some of the work the JavaScript runtime has to do to
      the browser's native rendering engine.

- Augment the compiler in any tool to **numerically evaluate** style
  rules with respect to media-queryable variables.  Do this covering
  some value region at some resolution, and emit media queries that
  select a rendition of the style sheet based on the nearest sample.

    - This goes anywhere that symbolic evaluation goes, to handle
      situations where the variable influences the answer in a
      continuous (rather than discrete) way.

    - Down sides: the resulting experience will be jaggy, and the
      process may emit large stylesheets with many media queries.

- Augment the compiler of GSS, Flapjax, or Elm to **partial-evaluate**
  the constraint system or the functional reactive network into the
  runtime system.  If this works, it should improve the runtime
  performance, perhaps considerably.  It may not work, however,
  because the constraint system or the functional reactive network may
  not be static enough.

- Incorporate (incremental) **linear constraint solving** from
  Cassowary into Stylus, SASS, Flapjax, or Elm, to handle linear
  constraint layouts in the context of any of those tools.

- Incorporate a **functional reactive engine** like Flapjax or Elm
  into GSS to handle reactivity to discrete variables.

- Incorporate **satisfiability solving** or **mixed integer-linear
  programming** into any of the above to extend their domains to
  handle linear and discrete constraint layouts.

    - It may be appropriate to limit the scope of discrete solving to
      constraints that depend only upon low-frequency variables.  Full
      mixed-integer linear programming may be too heavy-weight for
      interactive interfaces, but may be lightweight enough for
      responsive constraint-based layouts.

- Implement generation of **on-demand explanations** into anything
  that solves constraints.  Such explanations should be a godsend for
  debugging constraint layouts.

Notes
-----

[^css-functions]: It may be possible to implement this particular example
using the `calc` and `attr` functions from CSS3, if `attr` is willing
to return the value of a computed attribute like the element's height.
I did not manage to make it work in Firefox 23, however, so there is
scope for doing this in a preprocessor.  When `attr` becomes stable,
it could perhaps be retrofit as a backing technology.

    Also, the semantics of this particular example is that this
`#screen` should be the full width of the user's window regardless of
the width of its containing element; but CSS percentages don't do
that.  So, to make this happen without constraining the markup, `attr`
would need to be able to capture the value of the width of one element
and use it in a `calc`ulation in another element.  There may be a good
reason for `attr` not to be able to behave like that in any near
future at all: with such flexibility in `attr`, it becomes easy to ask the
browser to solve systems of equations, and retrofitting that ability
into existing layout engines may be quite difficult.  More on systems
of equations later.

[^disconnected]: In fact, it is quite possible that in the layout
setting, groups of constraints will even turn out to be completely
disconnected, so that even a complete explanation of what's going on
with a given variable may not need to mention the entire layout.
