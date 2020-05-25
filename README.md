<h1 align=center>
    Fluo
    <br>
    <a href="https://github.com/fluo-lang/fluo/actions"><img src="https://img.shields.io/github/workflow/status/fluo-lang/fluo/build-test-bench?style=for-the-badge" height=23px></img></a>
</h1>

<br>
<br>

> "Simplicity is the ultimate sophistication."
> <br>
> — Leonardo da Vinci

```rust
def fizzbuzz(value: int) {
    match loop 1..value {
        %% 15 -> print("Fizzbuzz");
        %% 3  -> print("Fizz");
        %% 5  -> print("Fizz");
        _     -> print(_);
    }
}

def entry() {
    print("Hello, 世界!");
    fizzbuzz(100);
}
```

```rust
def fib(n: int) -> int {
    if n <= 1 { 
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

def entry() {
    print(fib(46));
}
```

[Trello](https://trello.com/b/5gxtFXun/fluo)

# Usage

1. [Install LLVM](https://github.com/fluo-lang/fluo#Installing%20LLVM)

2. Install cargo and switch to nightly branch:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   rustup default nightly
   ```

3. Create an object file and link it (`clang` works as well):
   ```bash
   cargo run examples/tests.fl
   gcc out.o -no-pie
   ./a.out
   ```

4. There is no step 4

# Installing LLVM
Currently, you will need to install LLVM on your own. In the future, this will change

| Operating System                              | Install Command                                                                                                    |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Mac OS                                        | `brew install llvm`                                                                                                |
| Debian Method One (Recommended)               | `bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"`                                                              |
| Debian Method Two (If the above doesn't work) | `apt install llvm-10`                                                                                              |
| Arch                                          | [AUR](https://www.archlinux.org/packages/extra/x86_64/llvm/)                                                       |
| Windows + Other                               | [Prebuilt Binaries](https://releases.llvm.org/download.html#10.0.0), make sure to set proper environment variables |

