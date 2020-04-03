<h1 align = 'left'>Fluo</h1>

<!--<p align = 'center'>
  <img src = '/images/igloo_logo.png'>
</p>
-->

<p align = 'left'>A syntax oriented compiled programming language</p>

<br>

```rust
def fizzbuzz(value: int) {
    match loop 1..value {
        %% 3        -> print("Fizz");
        %% 5        -> print("Fizz");
        %% 5 & 3 -> print("Fizzbuzz");
        _           -> print(_);
    }
}

def entry() {
    print("Hello, 世界!");
    fizzbuzz(100);
}
```
