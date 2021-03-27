Fluo has functors (`Mappable`), applicatives (`Applicable`), and monads (`Composable`). This is how they are defined:

```rust
trait Mappable : 'f {
  dec map : ('a -> 'b) -> 'f 'a -> 'f 'b

  dec write : b -> 'f 'a -> 'f 'b
  let write : d v = fmap (const d) v
}

trait Applicable : 'app 
    where 'app : Mappable
{
    dec fill : 'a -> 'app 'a
    dec apply : 'app ('a -> 'b) -> 'app 'a -> 'app 'b
    let apply : f a = fmap (\f' -> fmap f' a) f
}

trait Composable : 'w
    where 'w : Applicable 
{
  dec chain: 'm 'a -> ('a -> 'm 'b) -> 'm 'b

  dec replace : 'm 'a -> 'm 'b -> 'm 'b
  let replace : m k = m >>= (\a -> k)

  dec with : 'a -> 'm 'a
  let with = pure
}
```

For example, the `Option` implementation:
```rust
impl Mappable : Option 't {
    let map : f None = None,
        map : f (Some x) = Some (f x)
}

impl Applicable : Option 't {
    let fill : x = Some x
}

impl Composable : Option 't {
    let chain : None _ = None,
        chain : (Some x) f = f x
}
```
