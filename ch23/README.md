## Chapter 22: State

### My Reading Notes

#### 23.1 State

We think of state as data that exists in addition to inputs and outputs of our functions,
which can potentially change after each function is evaluated.

#### 23.2 What is state?

The simplest state is on or off, like a light switch. In haskell, `State` captures the idea
of a state that might update with each computation. For in-place mutation there's `ST`, which
we'll see in later chapters.

#### 23.3 Random number

* `System.Random` is designed to generate pseudorandom values.
* `StdGen` captures two `Int32` values and is used to seed the next random number.
* `mkStdGen :: Int -> StdGen` can be used to make one.
* `next :: g -> (Int, g)`, where `g` is `StdGen`. The first output is the random value,
    the second is the new generator.
* `random :: (RandomGen g, Random a) => g -> (a, g)` is like `next`, just for non-`Int`s.
* `randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)` lets you get a random value between
    two values (inclusive).

Chaining the state to keep getting new next values can get tedious, and making it easier
is the point of the techniques in this chapter.

#### 23.4 The `State` newtype

`newtype State s a = State { runState :: s -> (a, s) }`

Note that this looks a lot like `random` from above.

#### 23.5 Throw down

`state :: Monad m => (s -> (a, s)) -> StateT s m a`

[RandomExample](s5_example.hs)

`evalState :: State s a -> s -> a`

Recall the difference between `replicate :: a -> [a]` and `replicateM :: Monad m => Int -> m a -> m [a]`

`randomIO :: Random a => IO a`

##### Exercises: Roll Your Own

My solutions are at the bottom of the running section [example](s5_example.hs)

### Meetup topic seeds

1. Why does `liftA3 (,,) f f f`, where `f :: State StdGen Int`, not produce triples of the same `Int`?
    This is `rollDieThreeTimes'` in section 5.
