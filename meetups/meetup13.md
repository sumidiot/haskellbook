## Meetup 13: Chapters 20 & 21 - Foldable and Traversable

October 8, 2018

Attendees: 4 + 1 remote
Current Members: 50 (1 new!). RSVPed: 4

### Discussions

Zach likes folds. "Folding, Splitting, Span". Instead of looking at a single thing at a time,
look at a sliding window, collapse them down based on a decision function. Nick has always found
the sliding window function in scala annoying, would like .sliding(2) to return a list of pairs.


Nick asked about the crazy parsing of `fmap length Just [1, 2, 3]`, finally decided maybe it was ok:

* `:t fmap` is `(a -> b) -> f a -> f b`
* `:t length` is `t a -> Int`
* `:t Just` is `a -> Maybe a`
* `:t [1, 2, 3]` is `[Int]`

So, when you start composing,

* `:t fmap length` you get `f (t a) -> f Int`

To then add `Just` on the end, you end up thinking of it as `Reader a (Maybe a)`, where `Reader` is `->`,
and so the `f` is `Reader a`, and `t` is `Maybe`, giving

* `:t fmap length` as `Reader a (Maybe a) -> Reader a Int`, or, using arrows,
    `(a -> Maybe a) -> (a -> Int)`

Then pass in `Just` and get `a -> Int`, applied to `[1, 2, 3]` giving 1. In fact,
`fmap length Just` is the same as `const 1`.


We worked through the 'library functions' exercise for `Foldable`, for `sum` and `maximum`. We did
`maximum` with `foldr`.


Also talked through the `S` chapter exercise, still not sure why it fails the `functor` checkers.
While we didn't write down the `Traversable` instance, we got close on the whiteboard.

