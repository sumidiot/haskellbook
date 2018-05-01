## Meetup 7: Chapters 10 & 12 - Folding lists, Signaling adversity

April 30, 2018

Attendees: 6.5
Current Members: 42 (2 new!). RSVPed: 7

### Discussions

I distracted the initial question of "let's try the myAny" exercise from
the chapter exercises, with his confusion:

You can work through `foldr const 0 [1..5] = 1` with equations, just replacing
with the definitions for `foldr` and `const`, and it happens in 1 call to `const`:

    `foldr const 0 [1..5]
       = const 1 (foldr const 0 [2..5])
       = 1`

Now suppose we change `const` to `countConst` to track how many times it is called:

    `-- add a second value to the result type (and thus second input) for 'number of times called'
    countConst :: Int -> (Int, Int) -> (Int, Int)
    countConst a (_,c) = (a, 1+c)`

I expected that `foldr countConst 0 [1..5] = (1,1)` based on
the evaluation above. However, it comes out as `(1,5)`. If you look very carefully,
`countConst` has to force enough of the second argument to be able to extract the count, `c`,
and so in the evaluation above it has to call the `foldr` and cause the recusion on down.

So then, the follow-up that we didn't quite work on was: how do you change things
so that you actually can count how many times the function is called in a `foldr`?
That's a fun one. Jonathan mentioned IORef as sort of a magic purity-breaking thing
we can probably use. Definitely worth playing with some more.

After that, we took a few minutes for folks to work through `myAny`.

Then we talked about exercise 5.d in section 10.5, because I didn't understand what
the problem was asking "Can the following every return a different answer:
`foldr (||) True [False, True]`?" seems like an oddly-worded question to me. Perhaps
a better phrasing is: "Can `let f xs = foldr (||) True xs` ever return anything besides `True`?"
To that question, the answer is no (besides `undefined`).

Then we worked through the other elements of section 10.5 problem 5. The one we had
most fun with was probably `foldr const 'a' [1..5]`. Jon recalled his solution of
"change `const` to `flip const`."

I then asked about the pattern that they were trying to call out about catamorphisms
in the chapter exercises. Before the meetup, I had decided that it had something to do
with "maps out" of the type, but hadn't put enough thought into it. During the meetup,
we talked through the 3 examples (`Bool`, `Maybe`, and `Either`), and made some guesses
what other ones might look like, like `data TS a b c = T1 a | T2 b | T3 c` or
`data US a b c = U1 a b | U2 c`. Basically, for each component of the sum, the catamorphism
they're suggesting is like "maps out of the component to some other type". Writing this
out a little more, I think I got to the following: catamorphisms from a type, `T`,
to another type, `Z`, are elements of `Hom(T,Z) x T -> Z`. There's a canonical element of
that set, evaluation. The `Bool` example, and the `Maybe`, show that there are more than
just that element, though, for example `bool' :: a -> a -> Bool -> a` could be defined as
`bool' a a' b = a`.

Now, the question I still haven't worked out is what's the "canonical" catamorphism
for lists? Or what's the relationship to `foldr`?

