## Meetup 4: Chapter 6 - Typeclasses

March 5, 2018

Attendees: 8
(same as last week, still better than original goal).
Current Members: 38 (1 new!). RSVPed: 8

### Discussions

Question: the typeclass instances are "scoped" just to a typeclass.
What about a method in two typeclasses, sort of the diamond problem in OO?

Right, the method is on the typeclass instance, so it prevents you from making
instances for a class being in both typeclasses.

We demonstrated the diamond problem, two typeclasses with
the same function in it causes issues. This is captured in various versions of
[../ch6/DiamondProblem.hs](DiamondProblem.hs), plus/minus (un)commenting some lines
and playing with [../ch6/DiamondProblem2.hs](DiamondProblem2.hs).

In Real World Haskell they demonstrate how to get around the
typeclass issues. There are ways to qualify which instance to use.

Nick mentioned demo of Orphaned instance he played with,
sort of "the other version" of the diamond problem, where you have two
instance of a class being in the same typeclass (vs two typeclasses
with overlapping namespaces, the issue above).

newtype vs data? You can replace newtype with data, and things
might well work. [wiki](https://wiki.haskell.org/Newtype).
Seems like newtype is probably generally for alias.
Actually, `type` is for aliasing.
[SO](https://stackoverflow.com/questions/2649305/why-is-there-data-and-newtype-in-haskell).
Seems similar to scala tagged types. Different from TypeTag,
which is a way around Java erasure, similar to RTTI (run-time
type information) in C++.

Some links we brought up during discussion:
* http://www.haskellforall.com/2017/10/advice-for-haskell-beginners.html
* http://dev.stephendiehl.com/hask/
* https://cstheory.stackexchange.com/questions/12524/a-mathematical-categorical-description-of-type-classes
