## Meetup 9: Chapters 12 & 13 - Signaling Advsersity & Building Projects

June 4, 2018

Attendees: 5
Current Members: 44 (1 new!). RSVPed: 4

### Discussions

We didn't really have much to talk about in Chapter 12, and most of us had only
made some progress into Chapter 13. It all worked out, though, because Ron
suggested that we just build a thing while we were meeting. Ideas we voted toward:

* Map generation
* slackbot
* Another I forget exactly but maybe had something to do with music/audio

We decided to do the slackbot, which proved to be a fun exercise. Ron hooked us up
with an API token for a new bot in the cville slack. We poked around the first few
results in Google for a Haskell slack api library, and settled on one called
[`slack-api`](http://hackage.haskell.org/package/slack-api).

We started by trying to create a new slack project. It took longer than I'd have
hoped, given that it's one of the steps of chapter 13, but mostly that came down to
(a) I think I have my setup somehow weird on my laptop, and was getting what appeared
to be version issues, (b) I forgot that the book used `stack new (projname) simple`,
instead of just `stack new (projname)`. Once I dug up the `simple` bit, things started
moving. We tried just copying the [echo-bot example](https://github.com/mpickering/slack-api/blob/master/example/EchoBot.hs),
which worked when we used the version from the git tag corresponding to the version
we depended on in our project.

At that point, we basically had a running echobot. I learned that `stack build`
is different from `stack build (projname)`, if by no other measure than build time,
I think related to stack build cleaning everything out of cache first. I found some
of the stack steps took a lot longer than I wanted, but maybe I was just being
impatient, or it was my first setup, or something. Also I basically had 0 idea what
I was doing, so probably was crazy inefficient in most things I tried.

Anyway, we dug around a little on the docs for `slack-api`, and modified the bot
to respond differently to different types of messages, but not real notable code
changes from the example.

We finished up by pushing to github: [currybot](https://github.com/cville/currybot),
which was fun. Even better, Ron was able to clone and build, and get the thing to work!


### Scheduling

We talked about scheduling the next meetup, and the shape of it again. We considered
continuing building a thing together, but there's still a core interest in the next
several chapters (Functors and onward!). There's a few vacation conflicting with the
next several weeks for a few of us, so we decided to take a hiatus for June, and meet
again the week after 4th of July. In the mean time, we might try a Google Hangout
at the normal time, get people together to keep hacking on the bot we made.

