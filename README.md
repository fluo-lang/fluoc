<h1 align = 'center'>Fluo</h1>

<!--<p align = 'center'>
  <img src = '/images/igloo_logo.png'>
</p>
-->

<p align = 'center'>A programming language</p>

<br>

```fluo
func fizzbuzz(amount: int) -> null {
    println -> match for 0..amount {
        %% 3 => "Fizz";
        %% 5 => "Buzz";
        %% 3 & 5 => "Fizzbuzz";
        _ => _;
    }
}

func entry() {
    fizzbuzz(100);
}
```
