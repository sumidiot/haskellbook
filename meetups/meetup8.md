## Meetup 8: Chapters 11 - Algebraic Datatypes

May 14, 2018

Attendees: 7
Current Members: 43 (1 new!). RSVPed: 6

### Meta Intro

We started with some meta discussion of options of how to keep going with this group.
We mostly decided to keep doing chapters, at least for now, but other options included:

* seminar style, each week it's somebody's responsibility to present a topic
* group project (shared git repo we work on outside the meetups?)
* maybe hack on a PR for some open source project when we meet?

The discussions seem to go well when folks are given a challenge problem, everybody
seems to engage in it, which is fun. I'll hope to be a few days ahead in the reading,
and maybe post a challenge problem ahead of time, or least be ready with some.
Generally the meetup discussion seeds I keep while I read are helpful.

### Discussions

Before the meeting, Jon shared what he'd found about the questions I asked last time,
counting lazy evaluation calls. In particular, `import Debug.Trace` lets you do fun
things like `let f' = (trace "executed") . const`, then `foldr f' z l` and shows only
one "executed".

I forgot to take notes during the meetup, so this is a bit brief.

I did ask about using pattern matching sort of in a guard statement, and Zach turned up
[Pattern Guards](https://wiki.haskell.org/Pattern_guard).

I also asked about specifying a type with a polymorphic constraint. We've got functions
with constraints (e.g., `Num a => a -> a`), what about types? Can I make a parametric type
that works for only some types? Like `Num a => data NumHolder a = NumHolder a`? Again,
Zach came through with the googling, this time into a quick line in the
[Learn You a Haskell!](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).
The syntax is `data (Num a) => NumHolder a = NumHolder a`, following the example above.
"However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations,"
says LYAH.

Another question: what would a non-algebraic datatype be? We pondered for a few minutes,
and Vivek realized that function types (exponentials) aren't algebraic (combinations of
sums and products).

Can you have a data constructor with validation? Yes!
[Smart Constructors](https://wiki.haskell.org/Smart_constructors).

We also talked about my confusion with the phone problem in the chapter exercises,
and I shared my work on the Vigenere exercise.

