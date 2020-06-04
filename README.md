<h1 align=center>
    <img src="img/logo_text.svg" height=300px/>
    <br>
    <br>
    <br>
    <a href="https://github.com/fluo-lang/fluo/actions"><img src="https://img.shields.io/github/workflow/status/fluo-lang/fluo/build-test-bench?style=for-the-badge" height=24px></img></a>
    <a href="https://trello.com/b/5gxtFXun/fluo"><img src="https://img.shields.io/badge/trello-here-000000FF?style=for-the-badge" height=24px></img></a>
</h1>

<br>
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
<br>

## Installation

1. [Install LLVM](https://github.com/fluo-lang/fluo#installing-llvm)

2. Install cargo and switch to nightly branch:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   rustup default nightly
   ```

3. Clone this repo (recursively):
   ```bash
   git clone --recurse-submodules -j8 git@github.com:fluo-lang/fluo.git
   ```

4. There is no step 4

<br>

## Usage
Make sure you're in the fluo directory:
```bash
cd fluo
```

Fluo generates object files and you need to link them (this will change):
```bash
cargo run examples/tests.fl
./a.out
```

<br>

## Installing LLVM
Currently, you will need to install LLVM on your own. In the future, this will change.

| Operating System                              | Install Command                                                                                                    |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Mac OS                                        | `brew install llvm`                                                                                                |
| Debian Method One (Recommended)               | `bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"`                                                              |
| Debian Method Two (If the above doesn't work) | `apt install llvm-10`                                                                                              |
| Arch                                          | [AUR](https://www.archlinux.org/packages/extra/x86_64/llvm/)                                                       |
| Windows + Other                               | [Prebuilt Binaries](https://releases.llvm.org/download.html#10.0.0), make sure to set proper environment variables |

