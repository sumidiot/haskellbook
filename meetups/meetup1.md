## Meetup 1: Introductions, Chapters 1 & 2

January 22, 2018

Attendees: 15 (my goal number!). Current members: 34.

### Pre-game and Introductions

There was some discussion while we had pizza on
[Church Numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals),
[SKI calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus),
and then
[Jot language and Iota](https://en.wikipedia.org/wiki/Iota_and_Jot).
Also mentioned was the
[Pairing function](https://en.wikipedia.org/wiki/Pairing_function) (set theory): (a,b) = {a,{0,b}},
and a recommendation to look at the work of
[Martin Davis](https://en.wikipedia.org/wiki/Martin_Davis).

While going through introductions, there was a suggestion that many folks might
be able to keep up with the concepts, if not the chapters/exercises. I suggested
I could try to be posting my notes ahead of time to give people a somewhat
shorter thing to read. Ron joked about offering rewards (haskell plushies!) for
folks who kept up with the reading. Ron also encouraged everybody to reach out
to him with other ideas for food (pizza is stereotypical, and delicious, but
doesn't fit everybody's diet goals or restrictions).

We had 15 folks from 13 different companies, which was awesome.

In addition to the haskell book (see the meetup group for the discount code!),
[Category Theory for Programmers]()
was also brought up. The
[printed version]()
got high recommendations from a few of us (myself included).

### Discussions (might be disjointed notes)

I missed the whole list, but one person rattled off a long list of things like
reduction, application, simplification, and asked if they were all actually the
same or if they had minor differences. Generally my take was they were the same.

alpha, beta, eta reductions are the code in lambda calculus

lazy evaluation vs strict

function vs combinator: combinator only depends on parameters. Some of the chapter
exercises related to this.

pure function = referential transparency

My take: just a symbol game, various transformations

There was a question of: ok, so you can go from lambda calculus to church
numerals. Then where do you go? Part of the point is to understand what
computation _means_. These abstractions are ways to rigorously build up to
richer theorems about computability and decidability and such.

An analogy was drawn along the lines of:
* imperative = turing machine
* declarative = lambda expressions
* Ron: functional != declarative

Some of the original motivation for some of the work around lambdas was the
the halting problem, computability. What does computability actually mean?

From a practical standpoint, though, it's very possible one could start in Chapter 2.
I'm curious to see where and how the lambda calculus as introduced in chapter 1
is brought back up throughout the book. There was a suggestion that it helps
with things like currying, and maybe it does for some folks, but I think you can
grok currying without chapter 1. Again, time will tell. An entertaining analogy
was drawn to replacing your language with 1 vowel and 1 consonant.

[Guy Steel junior]() has talk from 80s,
[growing a language]().

Python is broken: you can't do everything you'd do in a def as you would in a lambda.
(or maybe I got that backwards?)

Vivek presented his way of thinking about lambda calculus, which is much more
visual. He mentioned the
[de Bruijn notation](),
and brought up how `\x . xx` is called a mockingbird (repeats its input).
He also presented his visualization app, which we should try to get a link/video for.

The [y-combinator]() finds fixed points (`p` such that `f(p)=p`). It does this by
repeated application. You can simulate this by finding `cos` of something on a calculator,
and then cos of that, and cos of that, etc. Note that the y-combinator gives you
recursion - lambdas don't have names, so how could they call themselves?
There's also a
[z-combinator](),
which can be used for things like converting a tree graph to a linear structure.

The scala community has new compiler,
[dotty](),
which is sort of Odersky's experimental compiler but probably becoming scala 3.0.
He wanted to check if things added to scala were sound.
Foundation of dotty simplifies scala syntax (core), and he built a prover.
dotty comes from dot-calculus, dot = dependent object types.

Speaking of dependent types, Haskell doesn't have them by default, but they
are available as a compiler extension. Dependent types are where the type depends
on the value. I brought up the example of lists of a particular length, and how
the return type of `concatenate` takes into account the size of the input types.
And how you can show your compiler that `reverse` doesn't change the size of a list.
The
[idris book]()
was fun to read.

associatve destructuring: convert nested map -> value or set of values.
called a lens in haskell.
python calls those memory views.

strong vs weak typing argument? cs prof notes the battle has been raging for decades.
ron wants more of both. loves some of clojure.
sim: we were asking too little of compilers (tell everything types).
program that's small vs large, or team-based or not.

[strategic scala style guide]().
strongly typed language should look very different - write to refactor, because
the types permit. in weaker typed language, you replace types with tests. There's
a perception that tests that are hard to maintain or fragile. However, you _can_
write thtem later and they can be very targetted. Generative testing (or generating
the data to feed to tests) can be pretty awesome.

Question about clojure? Does it derive from closures? Yep. There are no
continuations though. set of bindings at instant in time.
