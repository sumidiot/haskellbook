## Meetup 5: Chapters 7 & 8 - More functional patterns and Recursion

April 2, 2018

Attendees: 6
Current Members: 40 (2 new!). RSVPed: 7

### Discussions

I noted missing that Num didn't imply Ord. Examples include complex
numbers, and cyclic groups (Z/n).

Y-combinator creates fixed points. The
[Lazy functional implementation](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) of the wikipedia
page provides a good example of factorial as the results of Y-combinator.

Why would you want to use the Y-combinator? Just for proofs?
You can also use it to find fixed points, if you're interested in
doing so. e.g., fixed point of `cos x`.

Jon told us about the difference between
[primitive recursion](https://en.wikipedia.org/wiki/Primitive_recursive_function),
and non-primitive recursion, e.g.,
[Ackerman function](https://en.wikipedia.org/wiki/Ackermann_function).

Vivek gave a quick introduction to category theory. You have
objects, O, and arrows, A, with source and target arrows, s,t:A->O,
identity arrows, u:O->A, and composable products, P={(a,b)\in A\times A|t(a) = s(b)}.
A functor is then a structure preserving mapping between categories,
and a natural transformation is a map between functors, represented
by an arrow T:O->A' (for categories C, C'). An example of a natural
transformation is head:List->Maybe.

An example of a category is Set, with objects the sets, and arrows
the functions between sets. There's some special objects, the empty
set, and the 1-element set (the set containing the empty set). The
empty set is "initial" because for any other set A, there's a (unique) map
\emptyset -> A. The 1-element set is final, there's always a (unique) map
to it from any set. Two useful constructs are the product,
A\times B={(a,b)|a\in A, b\in B}, and the disjoint union,
A\sqcup B={x | x \in A or x \in B}. Both are universal constructions,
and are dual to eachother.

After all that, a type constructor is a functor Set->Set, if you think
of sets as types. An example is: 1:X\mapsto \{\emptyset\}. Another is
the identity functor. The product and disjoint union are "bi"-functors,
functorial in each component. Anything that's a composition of these
is an algebraic data type. Product is the Haskell (,), disjoint union
is Either, 1 is Const ().

Bool = True | False is Either (Left ()) or (Right ()), 1\sqcup 1, 1+1=2.
Maybe x = 1 \sqcup x. List a = 1\sqcup (a\times List a) = 1 + x + x^2 +....
Tree x = x + Tree(x)*Tree(x).

species are the categorification of generating functions, so they're
very closely related to algebraic data types.

T(x) = x + T(x)^2 => T=(1\pm\sqrt(1-4x))/2 = \sum\frac{1}{n+1}\left(\choose{2n}{n}\right)x^n.

You can talk about cardinality of a category, and it's somewhat
different than the cardinality of a set (number of numbers). What's
the cardinality of the space of trees? There's some theory, and it
should be T(1). Plug that in the quadratic formula, it's (1\pm i\sqrt{3})/2.
Note that (T(1)^6)=1 (T is a 6-th root of unity). This also means
T^7=7, so Trees are the same as 7-tuples of trees
([paper](https://arxiv.org/abs/math/9405205)).

You can take derivatives of types. L' = L+L, because (1/(1-x))' = 1/(1-x)^2.
Derivatives are punching a hole in a type, if you punch a hole in a line
segment, you get two line segments.
There's probably some [fun links](https://pavpanchekha.com/blog/zippers/derivative.html).

What's the cardinality of the category of finite sets? You're supposed to
add up the reciprocals of all the bijections of a finite set, which is then
just the real number e.

\choose{n}{k} can be written without n in the denominator. After that,
you can write it with negative n, and you get (-1)^k\times\left\left( \choose{n}{k}\right\right), multi-choose (i.e., choose with replacement).
There's some physics analog, fermions and bozons.

