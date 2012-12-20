# semigroups-actions

In mathematics, an action of a semigroup on a set is an operation that
associates each element of the semigroup is with a transformation on the set.
See Wikipedia articles on
[semigroup action](http://en.wikipedia.org/wiki/Semigroup_action) and
[group action](http://en.wikipedia.org/wiki/Group_action).

## Examples

Similarly to semigroups, semigroup (or monoid) actions arise almost everywhere (if you look for them).

### Natural numbers acting on monoids

The multiplication monoid of natural numbers acts on any other monoid:

    n `act` x = x <> ... <> x -- x appears n-times

So `0 \`act\` x == mempty`, `3 \`act\` x == x <> x <> x` etc. TODO efficiency

This is expressed by `newtype Repeat` and instance

    instance (Monoid w, Whole n) => SemigroupAct (Product n) (Repeat w) where

Many different concepts can be expressed using such an action, including

- repeating a list `n`-times (`replicate`),
- compose a function `f` `n`-times, commonly denoted as _fⁿ_ in mathematics:

      (Repeat (Endo fⁿ)) = (Product n) `act` (Repeat (Endo f))

## Self-application

Any semigroup (or monoid) can be viewed as acting on itself. In this case, `act` simply becomes `<>`. This is expressed by the `SelfAct` type:

    newtype SelfAct a = SelfAct a
    instance Semigroup g => Semigroup (SelfAct g) where
        (SelfAct x) <> (SelfAct y) = SelfAct $ x <> y

## Matrices acting on vectors

Matrices with multiplication can be viewed as a group acting on vectors.

# Copyright

Copyright 2012, Petr Pudlák

Contact: [petr.pudlak.name](http://petr.pudlak.name/) or through github.

License: BSD3
