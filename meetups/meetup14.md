## Meetup 14: Chapters 22 & 23 - Reader and State

October 28, 2018

Attendees: 4
Current Members: 50 (Too many!). RSVPed: 4

### Discussions

Links:
* https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/
* http://bitemyapp.com/posts/2018-10-03-wrapping-up-haskellbook.html
* http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html
* https://www.youtube.com/watch?v=6WM4gFP7rs4
* https://en.wikibooks.org/wiki/Haskell/do_notation
* https://aphyr.com/posts/342-typing-the-technical-interview
* http://joelgrus.com/2016/05/23/fizz-buzz-in-tensorflow/
* https://youtu.be/HIXrbuwb1rQ?t=1200

`Reader` vs `ReaderT`? `Reader` is `ReaderT` where the monad you transform is `Id`.

`Reader` is somewhat similar to js `this`, in some sense.

Talked through Nick's confusion with the shawty exercise, and also the reader example
from the mtl docs.

Vivek talked about how Applicative's `f (a -> b) -> f a -> f b` is equivalent to
`(f a, f b) -> f (a,b)`, or `f a -> f b -> f (a,b)`. Proving the equivalence required using
all of the interesting structure of the category of Haskell types.

I asked about my `[Maybe a] -> Maybe [a]`, and eventually Vivek pointed out it'd never return
`Nothing`, so the function I was actually thinking of was `[Maybe a] -> [a]`. Hoogle does help
here, points out that `catMaybe` is such a function. Wondered about
[Alternative](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus).

