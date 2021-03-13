# Specification

This is the specification for the fluo programming language. The spec is not 100% complete, so if you have any questions feel free to contact me (e.g., through raising an issue on this repo).

For now, this document will mostly be for examples, though later I will expand it.

```python
# Create an identity function
let ident: a = a

# or more explicitly,
dec ident: a -> a
let ident: a = a
```

Functions can do pattern matches (must be exaustive):

```python
# Define our own map function
dec map: (a -> b) -> List a -> List b

let map: f [] = []
let map: f [x:xs] = [(f x) : (map f xs)]

# Define a "default" if None function
dec default: a -> Option a -> a

# an `_` throws away the value
let default: _ (Some val) = val  # use value in `Some`
let default: d None = d  # use default value

# Error, not exaustive pattern matches
let uhOh: None = None
# Missing `Just` case
# let uhOh: Just val = None
```

Pattern matching:

```python
# Equivalent to the example above, but with pattern matching in the function body
dec default: a -> Option a -> a
let default: d val = match val {
  Some v -> v
  None -> d
}

# Error, not exaustive pattern matches
let uhOh: val = match val {
  None -> val
  # We're missing
  # Just v -> <stuff...>
}
```

Sum data types (i.e., algebraic data types):
```python
rec Option : a = Some a | None

# The type is `Point`, but to init it would be `Point2D 0 0`
rec Point = Point2D {
  dec x : Int
  dec y : Int
}
```

Traits:
```python
# Value "stored" in partially applied function
dec const : a -> b -> a
let const : a _ = a

# Haskell-like Functors example
trait Functor : f {
  dec fmap : (a -> b) -> f a -> f b
  dec (<$) : b -> f a -> f b

  # Defualt implementation
  let (<$) : d v = fmap (const d) v
}

# Implement for option
impl Functor (Option t) {
  let fmap : f (Some a) = Some (f a)
  let fmap : _ None = None

  # Override the default implementation (equivalent)
  let (<$) : b _ = Some b
}

# Implement for list
impl Functor (List t) {
  let fmap : f [x:xs] = [(f x): (fmap f xs)]
  let fmap : _ [] = []
}
```

Currying:
```python
# Has signature `Int -> Int -> Int`
let add : x y = x + y
# Has signature `Int -> Int`
let add5 : other = add 5
# Has signature `Int`
let seven = add5 2
```
