Fluo has support for monads, applicatives, and functors. They are all traits:

```rust
trait Functor : 'f {
  dec fmap : ('a -> 'b) -> 'f 'a -> 'f 'b

  dec (<$) : b -> 'f 'a -> 'f 'b
  let (<$) : d v = fmap (const d) v
}

trait Applicative : 'app 
    where 'app : Functor 
{
    dec pure : 'a -> 'app 'a
    dec (<*>) : 'app ('a -> 'b) -> 'app 'a -> 'app 'b
    let (<*>) : f a = fmap (\f' -> fmap f' a) f
}

trait Monad : 'm
    where 'm : Applicative 
{
  dec (>>=) : 'm 'a -> ('a -> 'm 'b) -> 'm 'b

  dec (>>) : 'm 'a -> 'm 'b -> 'm 'b
  let (>>) : m k = m >>= (\a -> k)

  dec return : 'a -> 'm 'a
  let return = pure
}
```

For example, the maybe implementation:
```rust
impl Functor : Option 't {
    let fmap : f None = None
    let fmap : f (Some x) = Some (f x)
}

impl Applicative : Option 't {
    let pure : x = Some x
}

impl Monad : Option 't {
    let (>>=) : None _ = None
    let (>>=) : (Some x) f = f x
}
```
