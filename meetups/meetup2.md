## Meetup 2: Chapters 3 & 4

February 5, 2018

Attendees: 16 (better than my goal number!). Current members: 34. RSVPd: 12

### Discussions

We had a question about the rvrs exercise in chapter 3, but decided it was mostly
just an oddly worded question - you have to write the function, but it only has
to "work" for the one particular input. More on this below though.

Question about scoping: for a variable used in a function that isn't one of the
parameters, how does haskell know/decide if the variable is in scope or not, or
where does it come from? We weren't entirely sure, but expect the answer to be
that it's a static scope.

We talked about the difference between a type constructor and data constructor,
at least briefly. `data X` is a type constructor for the type `X`. When you write
that, and then `= other things` the other things are the data constructor, making
the data that fill up the type. In the example in the book,
data Mood = Blah | Woot`, `Mood` is the type, constructed by the type constructor
`data Mood`, and `Blah | Woot` is the data constructor, constructing the... data,
I guess, `Blah` and `Woot` that inhabit that type.

It was noted that it's hard to get infinite types easily this way. You get the
ints with Succ and Zero. We wrote out the definition for a tree, though this
was probably beyond the scope of the chapters so far:
`data Tree v = Node (Tree v) (Tree v) | Leaf v`

Finally, we worked on the extra credit problem to provide an actual answer to
the `rvrs` problem of chapter 3, which is now possible using the list operations
and pattern matching bits of chapter 4. The task is: write a function that
tokenizes a string (split on whitespace), and returns a string that is the
re-joined string after reversing the order of the list of tokens. We noted that
reversing a list isn't hard, and didn't actually talk about the join step during
the meeting, though it was brought up in published solutions after the meeting.
Instead, we mostly focused, during the meeting, on the exercise of writing a
`tokenize` function. My solution is in my [ec](../ch4/ec) folder for chapter 4.

