## Meetup 3: Chapter 5

February 19, 2018

Attendees: 8
(half last week, still better than original goal - President's Day lull?).
Current Members: 37 (3 new!). RSVPed: 8

### Discussions

Question/comment: why are typeclasses better, or when, vs things like java type hierarchies?
Not always better. Many factors. Seems like you're chopping up the 3 things of OO and representing
them separately. FP seems to try to make it so you sort of _have_ to do it the right way
(e.g., setting state in various places). Big impact on reasoning about programs, and testing,
and things like that.

How much can you replicate using Java interfaces in a good way using type classes?
Convenient to add a method to an interface, vs actually having a type class which is saying
"these things actually are really similar"

scala difference between trait and class is trait can't have constructor. java interface
can now have implementations, just can't have state. dotty = scala 3 will have constructors
in traits.

can we show examples of how to do a typeclass thing in various languages?
scala + java typeclasses aren't too bad too write, just a bit more verbose than typeclasses.

clojure provides a lisp dialect that captures the good way to do things and the lessons learned.
benefit of learning clojure is you learn from a better programmer than yourself.
clojure core has lots of good functions in it.
difference to haskell: you have to learn some of different way of thinking about code.

`foo :: Num a => a -> a`

```
trait Num[A] {
  def plus (a: A): A
}
def foo[A : Num](a: A): A
def foo[A](a: A)(implicit ev: Num[A]): A
```

```
String // standard library
implicit class StringIsNum extends Num[String] {
 def plus(s: String): String
}
```

is it like a mixin? it's ad-hoc polymorphism.
i think of mixins as more "traditional" trait-type inheritance/extension. typeclasses
factor out the evidence of being in the type as a separate step.

scala library: spyre, for lots of numeric types

`Show` is probably the best example to start with in Haskell. Need to `derive` it
to even print mew values in a user-defined type

is the pattern more important than the optimization?

it's probably the delegate pattern in Gang of Four.
scala's `implicit` keyword is what makes typeclasses better in scala than java.

Odersky's 2013 keynote,
["scala with style"](https://www.youtube.com/watch?v=kkTFx3-duc8), was really fascinating

how polymorphism works in clojure... "class" makes no sense.

python "dunder" methods (`__x`), build protocols. when you go deep in python, you find that
a class is a dictionary, bag of data and a bag of functions (bag = dictionary). you just call
a method on an object and see what happens. why am i calling this built-in function on my
object, instead of calling `x.whatever`. typically the built-ins are delegating to the "dunder"
functions. flask web framework turned everything into a mixin (e.g., requests have headers
and cookies mixed in).

scala has the `apply` magic.

can you write, say, a function that concatenates two things together, can you write that in
a way that it can concatenate things it hadn't seen? clojure (no longer jvm, you have
node-based clojure) has protocol classes, which are like `ISeq`, and you implement that.
"def record" in clojure is a bag of variables, and then you build up a way to tell "the reader"
about the polymorphic aspects of it. "when you see this method called, you're going to look
for every def method in the whole namespace, and ... call this one" (based on contents of
the map).

so... do you build up a library on top of that to make it look like these other
approaches? if your map doesn't have the key that a function is expecting, you get a Nil
values. so you do a lot of null checking. if you don't know destructuring, clojure is un-readable.

python has the `*args` functionality, scala has similar `:_*` for lists, but not tuples

fun to see haskell type inferencer
```
curry f a b = f (a, b) + 5
curry f a b = f (a, b + 8)
curry f a b = f (a / 7, b + 9)
curry f a b = f (a, b) ++ []
```

difference between `stack repl` and `stack ghck`? doesn't seem to be

phil's side-by-side language comparison table. similar to [hyperpolyglot](http://hyperpolyglot.org/)
(except they have scala, haskell, clojure in different tables)

any other polymorphic values? `()`  isn't `[]`.

scala functional libraries include `HList` (there's also KList and ohers)

http://degoes.net/articles/principled-typeclasses.
typeclasses don't allow specifying the laws.
coq allows you to get at that

scalaz and cats are fun.
library: discipline checks that the algebraic laws of monad (for example), to make it.
easy to determine if you are actually implementing all the required functions.

http://eed3si9n.com/ has lots of good cats writing
http://eed3si9n.com/herding-cats/

what is it that scala can do that haskell doesn't have? all these good ideas come out of
haskell, what are we missing/
* scalacheck can generate test samples, comes from quickcheck.
* easy to come from java. tooling. 
* scala is a good merge of functional and oo.
* kotlin is a nicer java, maybe easier for more folks to maintain.
* copy/modify/stumble-through is easier in some of the other languages.
* maybe the good parts are just moving out into other languages enough.
* any sense asking what the haskell roadmap has next?

Simon Peyton-Jones (appeal to expert)
[draws a diagram](https://www.youtube.com/watch?v=iSmkqocn0oQ) (perhaps):
c(unsafe) and haskell(safe),
and c(useful) and haskell(useless). Simeon doesn't like the tribalism and binarization.

Rich Hikey (clojure) uses the words simple and easy. but maybe "easy" things to start in get
harder to use (spaghetti code), but if you start simple, things get easier.

