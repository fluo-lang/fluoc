# Specification

This is the specification for the fluo programming language. The spec is not 100% complete, so if you have any questions feel free to contact me (e.g., through raising an issue on this repo).

For now, this document will mostly be for examples, though later I will expand it.

```rust
# Create an identity function
let ident: a = a

/# 
    A multiline comment
#/

# or more explicitly,
# 'a is a polymorphic type
dec ident: 'a -> 'a
let ident: a = a
```

Functions can do pattern matches (must be exaustive):

```python
# Define our own map function
dec map : ('a -> 'b) -> List 'a -> List 'b

let map : f [] = [],
    map : f (x~xs) = (f x) ~ (map f xs)

# Define a "default" if None function
dec default: 'a -> Option 'a -> 'a

# an `_` throws away the value
let default: _ (Some val) = val,  # use value in `Some`
    default: d None = d  # use default value

# Error, not exaustive pattern matches
let uhOh: None = None
# Missing `Just` case
# let uhOh: Just val = None
```

Pattern matching:

```python
# Equivalent to the example above, but with pattern matching in the function body
dec default: 'a -> Option 'a -> 'a
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
# note, const is just the name of the function, not a special keyword
dec const : 'a -> 'b -> 'a
let const : a _ = a

# Haskell-like Functors example
trait Mappable : 'f {
  dec map : ('a -> 'b) -> 'f 'a -> f 'b

  dec write : b -> f 'a -> f 'b

  # Defualt implementation
  let write : d v = map (const d) v
}

# Implement for option
impl Mappable : Option 't {
  let map : f (Some a) = Some (f a),
      map : _ None = None

  # Override the default implementation (equivalent)
  let write : b _ = Some b
}

# Implement for list
impl Mappable : List 't {
  let map : f (x~xs) = (f x) ~ (fmap f xs),
      map : _ [] = []
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

Conditionals:
```haskell
dec greaterThan5 : Int -> Int
let greaterThan5 : x = if x > 5 {
                           10
                       } elif x > 3 {
                           5
                       } else {
                           x
                       }
```

Let-in (local binding):
```haskell
let complicatedFunction : x = assign z = 10,
                                     y = x * 2
                                  in { z + y }
```

Custom Operators:
```haskell
let (+) : a b = add a b;

-- Lower number means the operator binds tighter,
-- for example * is usually lower than +
opdef (+) left binary 6
opdef (-) left binary 6

-- These operators are right associative
opdef (+) right prefix 3
opdef (-) right prefix 3

-- For example, a decrement operator
-- Operators with one argument are always prefix
-- Unless `postfix` is specified
let (--) : a = sub a 1;
opdef (--) left postfix 2

opdef (*) left binary 5
opdef (/) left binary 5

-- Concat operator
opdef (~) left binary 4
```
