## Meetup 12: Chapters 18 & 19 - Monad and Applying Structure

September 24, 2018

Attendees: 4
Current Members: 49 (2 new). RSVPed: 3

### Discussions

We worked through the `meh` chapter exercise. Vivek noticed that we're not actually using
the monad structure, actually just liftA2.

meh [a] f (a -> m b)
  -- mf : ma -> mmb, then join to get m b
  -- pure [a] gives m [a]
  -- [a] -> (a -> b) -> [b] -- is fmap for list
  -- apply m to that
  -- m [a] -> m (a -> b) -> m [b]
  -- m [a] -> (m a -> m b) -> m [b]
  -- now you've got m [a] above (pure [a]), join . mf :: ma -> mb, you put those in the function above

  -- (join . fmap f)
  -- liftA2 fmap



Peter shared [petermalcolm/hands-on-haskell](https://github.com/petermalcolm/hands-on-haskell)

