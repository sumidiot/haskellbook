## Chapter 8: Recursion

### My Reading Notes

#### 8.1 Recursion

Recursion means defining a function in terms of itself, so the function calls itself.

The lambda calculus doesn't have named functions, so how do you get recursion? You use
the Y-combinator, also known as the fixed-point combinator.

#### 8.2 Factorial!

Case expressions are useful for separating a base, non-recursive case, from the
otherwise infinite recursive case.

As a lambda calculus, all Haskell has is the notion of "apply" (there are just some
additional syntactic conveniences on top of that).

Recusion is similar to (and a type of) function composition, just that the functions
being composed are the same, and the number of times they are composed is dependent
on the input parameter. The book works through factoring out a function,
`applyTimes :: (Eq a, Num b) => a -> (b -> b) -> b -> b`, that calls a function a
fixed number of times, each time passing the result of the previous iteration in as
the argument for the next iteration.

#### 8.3 Bottom

The **bottom**, ⊥ (unicode 22A5), refers to computations that do not successfully
result in a value. That may mean fail with an error, or fail to terminate. This might
be an explicit `error` call, or a partial function (non-exhaustive case expressions).

`Maybe` is a type that lets you avoid the __bottom__. `data Maybe a = Nothing | Just a`.
Instead of writing a partial function, change the output type to a `Maybe`, and explicitly
return `Nothing` in the partial cases (and `Just` the answer in the others).

#### 8.4 Fibonacci numbers

This section builds up to the following implementation:

```
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)
```

Note that running this method on my laptop, with argument 100, took a while :).

#### 8.5 Integral division from scratch

Builds up to the "repeated subtraction" implementation of division:

```
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n) -- base case
         | otherwise = go (n - d) d (count + 1) -- recursive case
```

This shows the common Haskell idiom of a "__go function__" - the name of a function
defined in a where clause that takes more arguments than the outer function.

#### 8.6 Chapter Exercises

(below)

#### 8.7 Definitions

1. **recursion** means calling yourself

### Exercises

#### Chapter Exercises

##### Review of types

1. d
2. b
3. d
4. b

##### Reviewing currying

1. "woops mrow woohoo!"
2. "1 mrow haha"
3. "woops mrow 2 mrow haha"
4. "woops mrow blue mrow haha"
5. "pink mrow haha mrow green mrow woops mrow blue"
6. "are mrow Pugs mrow awesome"

##### Recursion

1. `dividedBy 15 2 = go 15 2 0 = go 13 2 1 = go 11 2 2 = ... = go 1 2 7 = (7, 1)`
2. `triang 0 = 0; triang n = n + triang (n - 1)`
3. `sumMult 0 _ = 0; sumMult n x = x + sumMult (n - 1) x`

##### Fixing dividedBy

In the [ChapterExercises](ChapterExercises.hs) source.

##### McCarthy 91 function

Also in the [ChapterExercises](ChapterExercises.hs) source.

##### Numbers into words

Yep, also in the [ChapterExercises](ChapterExercises.hs) source.

This exercises introduces `intersperse :: a -> [a] -> [a]`, from `Data.List`.
This function puts the first argument between neighbors in the second argument.
For example, `intersperse 'c' "this" = "tchcics"`, while `intersperse 'c' "a" = "a"`.

### Meetup topic seeds

1. Anybody read the [link](https://mvanier.livejournal.com/2897.html) from the footnote
    on the Y-combinator?
2. Anybody familiar with the interest in the
    [McCarthy 91 function](https://en.wikipedia.org/wiki/McCarthy_91_function)?
3. I found the let syntax in guards handy...
    ```
    | test,
      let whatever = whatever
      = answer
    ```
