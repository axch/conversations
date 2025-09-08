---
title: "Cleverness <em>of</em> Compilers <em>2:</em> How"
author: Alexey Radul
date: 2013/10/10
---

The [Cleverness of Compilers](/ideas/2013/cleverness-of-compilers/)
essay described the name of the hyperaggressive compilation game in
broad, philosophical strokes.  Here, I would like to walk through the
Mandelbrot example in some detail, so that the interested reader may
see one particular way to actually accomplish that level of
optimization.  READMORE There are of course other approaches to the
same goal, and the present approach (as well as its implementation)
leaves plenty of desiderata unfilled.  It is, nevertheless, one way to
do it.

As a reminder, the task is to turn this modular Mandelbrot program

```scheme
;;; Complex arithmetic library
(define (c:+ z1 z2)
  (cons (+ (car z1) (car z2))
        (+ (cdr z1) (cdr z2))))

(define (c:* z1 z2)
  (cons (- (* (car z1) (car z2))
           (* (cdr z1) (cdr z2)))
        (+ (* (car z1) (cdr z2))
           (* (cdr z1) (car z2)))))

(define c:0 (cons (real 0) (real 0)))

(define (magnitude z)
  (sqrt (+ (* (car z) (car z))
           (* (cdr z) (cdr z)))))

;;; Iteration library
(define (iterate count f x)
  (if (<= count 0)
      x
      (iterate (- count 1) f (f x))))

;;; Mandelbrot set membership test
(define ((step c) z)
  (c:+ (c:* z z) c))

(define (mandelbrot? c)
  (< (magnitude
      (iterate (real 400) (step c) c:0))
     2))
```

into this efficient, browser-executable one

```javascript
function fol_program(stdlib, foreign, heap) {
  "use asm";
  var heap_view = new stdlib.Float32Array(heap);
  var sqrt = stdlib.Math.sqrt;
  function operation_231(count, f_env_1, f_env_2, x_1, x_2) {
    count = +count;
    f_env_1 = +f_env_1;
    f_env_2 = +f_env_2;
    x_1 = +x_1;
    x_2 = +x_2;
    if (count <= 0.0) {
      heap_view[0] = x_1;
      heap_view[1] = x_2;
      return;
    } else {
      return operation_231(count - 1.0, f_env_1, f_env_2,
                           ((x_1*x_1 - x_2*x_2) + f_env_1),
                           ((x_1*x_2 + x_2*x_1) + f_env_2));
    }
  }
  function __main__(c_1, c_2) {
    c_1 = +c_1;
    c_2 = +c_2;
    var ret_x = 0.0;
    var ret_y = 0.0;
    operation_231(400.0, c_1, c_2, 0.0, 0.0);
    ret_x = +heap_view[0];
    ret_y = +heap_view[1];
    return (+sqrt(ret_x*ret_x + ret_y*ret_y) < 2.0)|0;
  }
  return __main__;
}
```

In the [DysVunctional Language
compiler](https://github.com/axch/dysvunctional-language), this task
happens in four steps:

[Flow analysis](#flow-analysis)

  ~ converts the input program into a form where all the
    specialization is explicit, and the contextual information
    relevant to each specialized code point is immediately available.
    In keeping with an unfortunate tradition, both the process and the
    result is called an "analysis".

[Code generation](#code-generation)

  ~ converts the result of flow analysis (without reference to the
    original program) into code in FOL, a First-Order intermediate
    Language, where all types and control flow are explicit (all
    function calls are to statically apparent targets).

[Further optimization](#further-optimization)

  ~ optimizes the intermediate FOL program with (aggressive versions
    of) standard methods, to which FOL is well suited because of its
    restricted nature.

[Backend translation](#backend-translation)

  ~ translates the optimized FOL program to asm.js syntax.  FOL is
    sufficiently low-level for this not to be too complicated.

The plan for this essay is to walk through all four of these
operations as they apply to this example program.

Flow Analysis
-------------

The flow analysis in DVL is an abstract interpretation of the input
program.[^cognoscenti]  An _abstract interpretation_ of a program consists of
executing (_interpreting_) the program, except that instead of the
variables and data structures holding the values the program actually
computes, they hold _abstract values_---representations of sets of
possible values that could occur during various runs of the program.
The idea is that one abstract value or abstract program state captures
some commonality of many possible actual values or program states
but ignores (_abstracts from_) the residual variation among them.

<blockquote class="pullquote-display"><p>
Reusable code is written to be able to consume huge
varieties of stuff, but any given use will only feed it some
restricted diet.</p></blockquote>

If we do a good job of picking what commonality to represent and what
variation to ignore, it is possible to compute a consistent model of
all possible executions of the source program.  This
model then gives information about what values each part of the program
may produce, and therefore what the consumers of that part may
have to consume.  Figuring that out is extremely valuable for
optimization because general-purpose, reusable code is written to be
able to consume huge varieties of stuff (that's why it's called
"general-purpose"), but any given use will only feed it some
restricted diet.  For instance, the `iterate` in our Mandelbrot
program is only ever told to iterate one specific type of function,
namely `(step c)` for various complex numbers `c`, even though it was
written to be able to iterate absolutely anything.  Discovering this use pattern
opens the door to a plethora of optimizations.

[^cognoscenti]: For those who know the jargon, I can elaborate a bit.
The trick in DVL is that the abstract value space is extremely
fine-grained: the only actual abstraction is over unknown real numbers
and unknown booleans (general sum types are on the roadmap but not
implemented yet).  In particular, the abstraction of a procedure is a
closure with a definite code body, and only the contents of the
environment are abstract.  The second trick is that the analysis is
polyvariant, and indeed there are no built-in bounds on the
polyvariance---all imprecision is introduced manually.  Eschewing
polyvariance bounds buys a certain predictability, namely the
generated code being exactly as specialized as the programmer intends.
The cost this choice pays, of course, is possible overspecialization,
including possible nontermination of the analysis.

How do we do abstract interpretation on DVL?  Since the DVL input
language is functional,[^gensyms] the state of execution of any
program is completely determined by the expression being evaluated and
the environment in which to evaluate it, or, alternately, by the
procedure being invoked and the arguments with which to invoke it.  We
can represent a whole class of executions of a DVL program by allowing
the values in such a state to be abstract; and we can simulate it with
a suitably modified interpreter that respects and appropriately
propagates the variation the abstract state permits, while deducing as
much as possible from the commonalities among states that the
abstraction exposes.

Without further ado, the following slideshow illustrates how flow
analysis by abstract interpretation proceeds on (an interesting part
of) our example `mandelbrot?` program.

[^gensyms]: Actually, DVL includes a primitive nullary procedure named
`gensym`, different dynamic invocations of which make distinct
objects.  This little bit of impurity is what puts the Dys- into
DysVunctional Language.  Some impurity is necessary to correctly
implement [automatic
differentiation](../introduction-to-automatic-differentiation/), and I
chose generating unique objects because they can still be accommodated
within the framework of flow analysis.  How to actually accommodate them is
beyond the scope of this footnote; but the program being described in this
essay is preserved in the DVL source repository as [Vunctional
Language (VL)](https://github.com/axch/dysvunctional-language/tree/master/vl/).

<center>
<iframe class="frame" width="540" height="590" src="embedder.html#animated-analysis.html">
  <a href="/animated-analysis.html">the analysis as a standalone
  page</a>
</iframe>
</center>

<p></p>

At the end of the flow analysis process (it ends on the Mandelbrot
example, but in general DVL relies on correct use of the `real`
primitive to ensure that abstract evaluation terminates), we have an
analysis data structure holding a bunch of information like this:

$$ \begin{eqnarray*}
\exp{(iterate (real 400) (step c) c:0)}, \env{1} & \mapsto & \RR \\
\obj{iterate}, \R, \obj{closure1}, \RR & \mapsto & \RR \\
\exp{(<= count 0)}, \env{2} & \mapsto & \B \\
\obj{closure1}, \RR & \mapsto & \RR \\
\textrm{etc} & &
\end{eqnarray*} $$
These entries tell us that 

- The expression $\exp{(iterate ...)}$, evaluated in any environment
  of the same shape[^shape] as $\env{1}$, should be treated as though it
  returns a pair of arbitrary real numbers;

- The procedure $\obj{iterate}$, when applied to any triple of
  arguments consisting of any real number, any procedure of the same
  shape as $\obj{closure1}$, and any pair holding any two real
  numbers, should be treated as though it returns a pair of arbitrary
  real numbers;

- The expression $\exp{(<= count 0)}$, evaluated in any environment
  of the same shape as $\env{2}$, should be treated as though it
  returns an arbitrary boolean;

- etc.

[^shape]: By "shape" I mean that $\env1$ or $\obj{closure1}$ are
actually abstract, and may, for example, declare some variables bound
to arbitrary real numbers rather than to specific values, allowing
variation at those points.  Note that in this system, abstract
procedures like $\obj{closure1}$ still have just one concrete
procedure body, and admit variation only in the values of closed-over
variables.  This fine granularity is important, because application of an abstract
procedure means branching to a specific piece of code: all variation
in control flow caused by higher-order functions is at this point
solved.

Moreover, the entries tell us exactly the shapes of program points
through which execution will pass, so they carry sufficient
information to generate code that will do the same thing but with
explicit data types and control flow, which will then be
optimizable by standard methods.

Code Generation
---------------

Now that we have a flow analysis in hand, what do we do with it?
The entries in the analysis cover all the shapes of all the program
points that will be encountered during program execution.  In
particular, the entries like

$$ \obj{iterate}, \R, \obj{closure1}, \RR \mapsto \RR $$
correspond to procedure applications, and cover all the function calls
(in all the variations on the shapes of their arguments) that will
occur at runtime.

We generate a procedure corresponding to each such entry.  Since a given
source procedure may lead to many entries if it is called with arguments of
many different shapes, this is the place where we materialize the
specialization that the flow analysis gave us.  The body of
each generated procedure is bascially a direct copy of the body of the
closure being applied, with three important exceptions:

- Any subexpressions whose concrete values are completely determined
  by what is known about the argument shapes get replaced by constants
  (in particular, a lot of the type tests that happen in the source
  language go away at this point).

- To make sure the result is first-order, the code generator performs
  [closure
  conversion](http://matt.might.net/articles/closure-conversion/);
  that is, every `lambda` form becomes a constructor for an explicit
  data structure, and every procedure accepts and deconstructs this
  structure to find the variables it was closed over.  Note that we
  don't need to keep track of the code pointer, because the abstract
  evaluation already computed where they flow.

- Each application in the body becomes a call to the appropriate
  generated procedure.  The resulting static control flow information
  is very precise because we always call exactly the specialization
  determined by the shapes of the arguments passed at that particular
  call site.

For example, the entry

$$ \obj{iterate}, \R, \obj{closure1}, \RR \mapsto \RR $$
becomes the following code (comments added):[^elided]

```scheme
(define (operation-231  ; iterate-R-cl1-R.R would be a better name
         the-env        ; arg introduced by closure conversion
         count f-env x) ; original args, but proc now just env
  ;; The intermediate language has explicit type signatures
  (argument-types
   env-226                    ; environment data type
   real env-235 (real . real) ; types of formal parameters
   (real . real))             ; return type
  (if (operation-234 ; application of <= to R and 0
       (env-226-g:<= the-env) ; the <= procedure is in my closure
       count 0)
      x
      (operation-231 ; recursive application (same arg shapes)
       the-env       ; recursion means same closure
       (operation-24 ; application of - to R and 1
        (env-226-g:- the-env)
        count 1)
       f-env
       (operation-23 ; the call (f x) from the source
                     ; the target is static, and itself specialized
        f-env x))))
```

[^elided]: Actually, if you look in [the
source](https://github.com/axch/dysvunctional-language/blob/master/vl/code-generator.scm),
this output is somewhat simplified from what code generation produces, but
not in any important way.  I have post-processed the output to
eliminate a few confusing and uninteresting artifacts of the choice of
minimum primitive basis in the DVL language, and of the details of DVL
macroexpansion into said minimum primitive basis.  In particular,
recursion is actually handled with a variant of the [Y
combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator)
rather than explicit implementation as this example would suggest.
But that's beside the point.

Are we fast yet?  No, I expect not.  But though this may not be
apparent by looking at it, this intermediate code is much more
amenable to optimization by well-known methods than the

```scheme
(define (iterate count f x)
  (if (<= count 0)
      x
      (iterate (- count 1) f (f x))))
```

we started with.  For instance, the minus procedure (spelled `-`) is not actually
primitive in DVL, but defined in the standard library as a generic
procedure that operates on arbitrary "vector-like" structures,
including trees of dual numbers for [automatic
differentiation](../introduction-to-automatic-differentiation/).
But the generated procedure `operation-24` is specialized to the case
of applying minus to some real number and the constant 1.  With further
optimization, `operation-24` will turn into a single machine
instruction, even though minus was capable of an arbitrarily deep
recursive computation.  But perhaps even more important in this case
is that the `(f x)` call from the source, which called a syntactically
unknown function, now becomes a static call to a known procedure
(namely, `operation-23`), which is itself specialized to its calling
context.

<blockquote class="pullquote-display"><p>Flow analysis
removes obstacles to optimization by detecting which
parts of the program don't actually communicate with each other,
so their interfaces need not remain compatible.</p></blockquote>

Why will this help us be fast later?  The primary obstacle to
optimization is the inability to
alter interfaces, because to alter an interface one needs to adjust
all the producers and consumers of that interface.  Flow analysis
reduces this obstacle by detecting which
parts of the program don't actually communicate with each other,
so that their interfaces need not remain compatible.
The generated FOL program captures this more accurate model of the
source program's communication flow, and is consequently very amenable
to optimization by standard methods.

Further Optimization
--------------------

Here is not the place to discuss in detail all of the optimization
passes that operate on intermediate FOL programs.  Suffice it to say
that aggressive application of the following standard techniques
produces quite pleasant results:

[Procedure inlining](https://en.wikipedia.org/wiki/Inline_expansion)

  ~ inserts (many) definitions of called procedures inline into their
    callers.  Inlining is less critical for FOL than for other languages
    because those called procedures are already quite specialized, so
    copying them yields less benefit.  Some later optimizations are
    still easier to spot after inlining than before, though.

[Common subexpression elimination](https://en.wikipedia.org/wiki/Common_subexpression_elimination)

  ~ replaces repeated computations by reuses of past results.  This
    pass also applies algebraic simplifications like `(car (cons x _))
    => x` and `(* z 0) => 0`.[^floats]  These activities are
    intermeshed in one pass because these optimizations
    cascade with each other.

[^floats]: Yes, yes, I know that converting `(* z 0)` to `0` does not
preserve semantics strictly in the presence of floating point
infinities and NaNs.  This optimization is present in the prototype
because it chains, and because the implementation of automatic
differentiation in DVL introduces lots of multiplications by
statically apparent zeroes.  In the limit as DVL approaches actual
production use, thought will need to be directed to the distinction
between code expressing formulas over nice mathematical structures
(where multiplying by zero always does produce zero) and code
carefully written with floating point semantics in mind.  This
actually strikes me as a very deep issue, because it is the difference
between specification and approximation.

[Dead code elimination](https://en.wikipedia.org/wiki/Dead_code_elimination)

  ~ deletes computations of values for unused variables.  The
    intraprocedural version is simple and effective.  The DVL compiler
    also implements an interprocedural version, which is mostly good
    for flushing stuff carried around but not used in recursive loops.

Scalar replacement of aggregates

  ~ converts compound data structures into sets of scalar variables
    when possible, reducing allocation and referencing.

[Constant folding](https://en.wikipedia.org/wiki/Constant_folding)

  ~ is not necessary because the code generator already replaced
    computations of statically known values with those constants.

In the case of our running example, the result after applying these
passes is the cleanest iteration for which one could wish:[^tail-recursion]

```scheme
(define (operation-231
         count
         f-env-1 ; The only variable content of the closure
         f-env-2 ; of f was two real numbers
         x-1
         x-2)
  (argument-types
   real real real real real
   (values real real)) ; Returns two values to avoid making a pair
  (if (<= count 0)     ; Primitive numeric <= now
      (values x-1 x-2)
      (operation-231   ; Loops still rely on tail recursion
       (+ count -1)    ; Primitive numeric +
       f-env-1
       f-env-2
       ;; Here is our elaborate arithmetic specialized to complex
       ;; numbers and inlined.  No allocation or generics here;
       ;; just floating adds and multiplies.
       (+ (- (* x-1 x-1) (* x-2 x-2))
          f-env-1)
       (+ (+ (* x-1 x-2) (* x-2 x-1))
          f-env-2))))
```

[^tail-recursion]: FOL currently has no explicit syntax for loops and
represents them as tail recursions.  This makes it difficult to
implement loop optimizations over FOL; but my philosophy is to let the
compiler targeted by the backend deal with that for now.  FOL's other
optimizations are quite useful for making sure the code generated from
FOL doesn't violate any hidden assumptions of its targets, but
optimizing loops inside FOL does not seem necessary for that.  On the
other hand, handling loops well may be a good idea if I get serious
about targeting asm.js, because it may not optimize said loops for me.

All that remains is to print it out in the backend's preferred syntax.

Backend Translation
-------------------

Translation from optimized intermediate code to the syntax of the
desired backend is not actually completely trivial, because backends
tend not to have exactly the same semantic model as the intermediate
language.  For example, the asm.js backend implements multiple value
returns by writing to the global array that serves as heap (I am
fortunate that I do not need it for anything else right now).  That
said, there isn't that much enlightening content in backend
translation as it currently exists in the DVL system.  Getting from
the optimized FOL in the previous section to the following asm.js is
really pretty straightforward.

```javascript
function operation_231(count, f_env_1, f_env_2, x_1, x_2) {
  count = +count;
  f_env_1 = +f_env_1;
  f_env_2 = +f_env_2;
  x_1 = +x_1;
  x_2 = +x_2;
  if (count <= 0.0) {
    heap_view[0] = x_1;
    heap_view[1] = x_2;
    return;
  } else {
    return operation_231(count - 1.0, f_env_1, f_env_2,
                         ((x_1*x_1 - x_2*x_2) + f_env_1),
                         ((x_1*x_2 + x_2*x_1) + f_env_2));
  }
}
```

And that's it.  There you have it, folks: the anatomy of one compiler
that's too clever for its own good.

<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  TeX: {
    Macros: {
      R: "{\\mathbb{R}}",
      B: "{\\mathbb{B}}",
      RR: "(\\R\\ . \\R)",
      eps: "\\varepsilon",
      exp: ["{\\textrm{#1}}",1],
      env: ["{\\textrm{env#1}}",1],
      obj: ["{\\left<\\textrm{#1}\\right>}",1],
    },
    equationNumbers: { autoNumber: "AMS" },
    noErrors: { disabled: true },
  }
});
</script>

Notes
-----
