# Fluo

Currently a WIP, with a mix of syntax from Haskell, Rust, and Go:

```python
import std.string.Show;

# Algebraic data types are a thing
# Like `Option<T>` in Rust, or `Maybe a` in Haskell.
rec Maybe[T] = Some T | Nothing;

# Create a typeclass called `MyShow`
# A typeclass that converts a value into a `String`
class MyShow[T] {
    fun my_to_string(T) :: String
}

# Implement `MyShow` for Maybe, where the value of `Maybe` implements `Show` (not `MyShow`!)
instance MyShow [Maybe[T: Show]] {
    fun my_to_string(maybe) {
        pattern maybe {
            # We can use `.to_string()` on the `val` because it implements `Show`
            Maybe.Some val -> "Some " ++ val.to_string()
            Maybe.Nothing -> "Nothing"
        }
    }
}

# Take any value that implements the `MyShow` typeclass
# The return type `Result[IOErr, ()]` is inferred here
fun my_print[T: MyShow] (val: T) {
    print(val.my_to_string())? # IO may fail, so we use the `?` to propagate the error
}

fun entry () {
    my_print(Maybe.Some 10)? # Prints "Some 10"

    # Error because our `Maybe` doesn't implement `Show` that we constrained
    # Our `Maybe` only implements `MyShow`
    my_print(Maybe.Some Maybe.Nothing)?
}
```

## Design

- Simple
- Embedded DSLs (powerful syntax additions)
- No garbage collector, but it feels like there is one (based off [ASAP: As Static As Possible memory management](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf) and [Practical Static Memory Management](http://nathancorbyn.com/nc513.pdf))
