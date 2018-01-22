## Chapter 1

### My Reading Notes

pure functional programminng = referential transparency

**lambda terms** expressions, variables, abstractions
* **abstractions** = functions, head (lambda) and body, and is applied to an argument
* head = \lambda then variable name (parameter). body is another expression
* `\x . x`

**alpha equivalence** - different variable names, same expression shape
* `\x . x   \alpha=  \y . y`

**beta reduction** - apply the function
* `(\x . x)(2) = 2`

function applications are _left associative_

**free variable** - variable in body that isn't bound by the head
* alpha equivalence doesn't apply to free variables, `\x . xz  !\alpha=  \x . xy`

**currying**
* `\xy . xy` is shorthand for `(\x . (\y . xy))`

Reduction beginning on page 18 needs more parens or something
* `(\xyz . <xz(yz)>)(\mn .m)(\p . p)`
* `[(\x . \y . \z . <xz(yz)>)(\m . \n . m)](\p . p)`
* `[\y . \z . <(\m . \n . m)z(yz)>](\p . p)`
* `\z . [(\m . \n . m)(z)]((\p . p)z)`
* `\z . (\n . z) [(\p . p)z]`
* `\z . (\n . z) z`
* `\z . z`

Actually, easiest to write it out as function of 3 arguments, with 2 applied:
* `(\xyz . [xz(yz)]) (\mn . m) (\p . p)`
* `[x := (\mn . m), y := (\p . p)`, so we're left with function of one argument, z
* `\z . [(\mn . m)z((\p . p)z)]`, inside[] is function of 2 arguments, with 2 applied:
    * `(\mn . m) (z) ((\p . p)z)`, this function is projection to first argument
    * `z`
* `\z . [z]`

#### Equivalence Exercises

1. b
2. c
3. b

**beta normal form** - can't beta reduce further

**combinator** - lambda with no free variables
* can't inject any new values or random data

**divergence** - reduction never terminates
* \omega term: `(\x . xx)(\x . xx)`

#### Chapter Exercises

Combinators: 1, 3, 4
Normal form: 1 is in normal form, 2 diverges (\alpha= \omega), 3 normal form is zzz
Beta:
    1. z: `(\wv .w)(z)(z)`
    2. bb: `(\a . z)bb`
    3. qq: `(\x . xx)(\z .zq) = (\z . zq)(\z .zq) = (\z . zq)q = qq`
    4. yy (same as 3)
    5. yy: `(\y . y)yy = yy`
    6. aac: `(\b . ba)(\b . ba)c = (\b . ba)ac = aac`
    7. \z . za: `\z . [((\x.z)(z))((\x.a)(z))]`


### Meetup Discussion

#### My conversation starters

1. Answer to Combinator.3 says: "in `\xyz . zy(zx)` the head is `\xyz.` and the body is `zy(zx)`".
   Why is it not `[\xyz . zy](zx)`? And would that mean [x := zx] or [x := z, y := x]?
2. The `z1` answer in 7 shows some entertaining subtlety
