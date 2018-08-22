## Meetup 11: Chapter 17 - Applicative

August 20, 2018

Attendees: 4
Current Members: 47 (3 new!). RSVPed: 4

### Discussions

Nick asked Vivek if he'd read the follow-up resources. Vivek had looked at the
McBride paper, "introducing" applicatives in ~2008, and tracked it down to MacLane,
in the 1950s, as a "monoidal functor".

Jon notes that its interesting that Applicative means you have a way to "wrap" a function.
Wonder if syntax trees, where inner nodes are operators and leaves are values, so you
can think of the functions as applying.

It seems like a binary tree might be an example of a functor that is not applicative.
Vivek notes that a way to think about it is: given a tree of one type, and a different
tree of another type, how do you make a tree of the tuple type?

Links:

1. https://stackoverflow.com/questions/6798699/monad-instance-for-binary-tree
2. https://mail.haskell.org/pipermail/beginners/2010-March/003856.html
3. https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/

Ha! Fun! Different notions of binary trees have different characteristics of where values
are - at all nodes, just at leaves, or just not binary.

Jon brings up a different structure... if a left tree has functions, and a right tree
has values... every coordinate in a tree is a string of "l"s and "r"s, if you have a node
on the left, and a node on the right tree, then you have two strings of "l"s and "r"s, and
so produce a new coordinate in a tree. However, you'll have collisions, "l"+"" = ""+"l",
so you need a way to combine values when that happens. However, you're combining values
of the output type of the function, so it depends on the function, so isn't very natural.

Vivek brought up the Day convolution, somehow related to applicatives.

Links: https://ncatlab.org/nlab/show/Day+convolution

Monad is monoid in endofunctors under composition. Applicative is monoid in endofunctors
under Day convolutions. Vivek's still trying to unwind those definitions for the categories
of sets.

https://arxiv.org/pdf/1803.05316.pdf

Brief tangent on adjunctions. Vivek shows how (-,b) : Type <-> Type : (b -> )
form an adjoint pair, that you can do the loops from left to right and back, or vice-versa,
and how you get maps to/from the identify. And that you can draw string diagrams that
capture the various laws of adjunction, and do string diagram chasing for proofs. And then
some things about quantum fluctuations.

We worked through why `ZipList` actually _is_ an applicative, like the book suggested,
and I had just gotten the answer wrong. The action is zipping, the interesting thing, which
I hadn't pieced together from the reading, despite the section for it, was that the `pure`
wasn't the empty list, as in the case of `[]`. Vivek pointed out that "applicative" means
"unital monoidal functor", where the "unit" natural transformation `id->F` gives you the `pure`.

