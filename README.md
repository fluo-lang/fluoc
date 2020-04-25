# Fluo

> "Simplicity is the ultimate sophistication."
> <br>
> — Leonardo da Vinci

```rust
def fizzbuzz(value: int) {
    match loop 1..value {
        %% 3        -> print("Fizz");
        %% 5        -> print("Fizz");
        %% 5 & 3    -> print("Fizzbuzz");
        _           -> print(_);
    }
}

def entry() {
    print("Hello, 世界!");
    fizzbuzz(100);
}
```
