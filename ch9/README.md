## Chapter 9: Lists

### My Reading Notes

#### 9.1 Lists

Lists can be either finite or infinite.

#### 9.2 The list datatype

List is `data [] a = [] | a : [a]`. `[]` is the type constructor, as well as the data
constructor for the empty list. The infix operator `:` is typically called __cons__,
short for __construct__. It is a __product__ data constructor, because it takes two
arguments (we'll learn more about sums and products later).

#### 9.3 Pattern matching on lists

You can mattern match on the head and tail of a list, as you would for other data
constructors: `let (h : t) = myList` pulls off the head element as `h` and the
tail list as `t`. You do have to be careful to match the empty list.

The `Maybe` type is useful for functions that __might__ return a result. It is
`data Maybe a = Nothing | Just a`. We __should__ think of `head : [a] -> a` as really
being `safeHead: [a] -> Maybe a`, because `head []` doesn't return anything sensible
(using `Maybe`, it would return `Nothing`). Later we'll learn more about `Maybe`,
as well as `NonEmpty`, which is a container with at least one thing in it.

#### 9.4 List's syntactic sugar

Instead of carefully "cons-ing" up a list, `1 : 2 : 3 : 4 : []`, Haskell lets you
just write `[1,2,3,4]`. We may talk about a "cons cell", being a space that values
may inhabit in the list, and the "spine" which is the thing that holds cons cells
together.

#### 9.5 Using ranges to construct lists

You can easily construct a list with a range syntax: `[1..10]`. Note that if you
specify the first two elements, it creates the range by stepping by the difference
each time. So `[1,3..10]` is the odd integers less than or equal to 10. You can
also make ranges of characters, `['a'..'z']`. You can have negative steps, but
if you ask for a negative step and the end of your range is greater than the beinning,
you'll get an empty list: `[1,-1,10]=[]`.

This syntax is supported by the functions

* `enumFrom`,
* `enumFromThen`,
* `enumFromTo`, and
* `enumFromThenTo`,

all of which require an `Enum`. For example, `enumFromThenTo 1 3 10` is the same
as `[1,3..10]`. Note that the first two methods, `enumFrom` and `enumFromThen`
produce infinite lists (assuming the underlying `Enum` type is infinite).

### Exercises

#### 9.5 Using ranges to construct lists

Captured in [source](s9_5EnumFromTo.hs).

### Meetup topic seeds

1. "cons cells" and "spines" don't seem to mean much to me. Anybody else?
2. How did you all do the EnumFromTo exercises in section 9.5? Did you use `succ`?
