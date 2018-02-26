## Scope Experiment

The purpose of this module is to play with the statement "there's only one instance of
a typeclass for a given type." There's no way to universally enforce this, in the sense
demonstrated by [ScopeMain1](scopeMain1.hs) vs [ScopeMain2](scopeMain2.hs), which import
different instances for the same types and typeclasses. However, as expected,
[ScopeMain3](scopeMain3.hs) fails, because the compiler sees more than one instance.

To try these examples, I used `ghc --make (file.hs)`, which produces an executable
`file` (if successful). Note that for this to work, the module has to be called `Main`.
