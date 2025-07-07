# vatch

> :warning: This is **prealpha** software. Even I do not use it for anything
> serious.

A Clojure library for pattern m**atch**ing on **v**arant types represented at
**v**ectors.

`vatch` provide value matching; true and full pattern matching is a non-goal.

## Why?

I spent too much time in Haskell land, and now I like modeling _some_ problems
in the form of variants (in the sense of [this talk][variants]) and pattern
matching (even though that [makes Rich Hickey cry a bit][sme]).

You don't need a library to make a variant value, but variants only really
shine when you can pattern match on them. This is what this library is for:
pattern matching on variant values.

### Why not core.match?

[`core.match`][match] exists. It's been designed mostly as a showcase for
[`core.logic`][logic] and implements powerful pattern matching semantics over
Clojure values, with advanced compile-time optimizations. It's great. If you're
even remotely curious about pattern matching in Clojure, go and try it.

It has a few things I dislike, though, and they have annoyed me enough that I
decided to try and build my own instead. Specifically:

- I believe `core.match` predates most Clojure destructuring syntax, or at
  least their widespread use. Regardless, it has its own destructuring syntax
  that does not match Clojure core (`let`) syntax, which I have found confusing
  at times. Specifically, matching map keys is done in the reverse order.
- `core.match/match` patterns will capture existing symbols in their
  environment. The meaning of `(match [x] [v] true :else false)` will depend on
  whether `v` exists in the lexical scope of this expression. I have spent hours
  debugging the effect of that; having a singular syntax that can either mean
  capture and check for equality _or_ introduce a new binding, depending on
  context, is just not compatible with my brain. The third time this happened is
  when I decided to write my own.

## What?

```clojure
(vatch thing
  [:tag data] (do-something-with data)
  [:other-tag data more-data] (do-something-else-with data more-data))
```

A variant value is a vector whose first element is a keyword (preferably
namespaced, but that's really up to you), and whose other elements are dictated
by that keyword. You should probably document some sort of schema for the
variants you expect, though that's outside the scope of this library.

## Usage

Todo.

[variants]: https://www.youtube.com/watch?v=ZQkIWWTygio
[sme]: https://www.youtube.com/watch?v=SxdOUGdseq4
[match]: https://github.com/clojure/core.match
[logic]: https://github.com/clojure/core.logic
