<div align=center>
    <!--<img src="images/logo.png" width=200 align=center></img>-->
    <h1>Fluo 🌊</h1>
    <a href="https://github.com/fluo-lang/fluo/actions"><img src="https://img.shields.io/github/workflow/status/fluo-lang/fluo/build-test-bench?style=for-the-badge" height=24px></img></a>
    <img src="https://img.shields.io/tokei/lines/github/fluo-lang/fluoc?style=for-the-badge" height=24px></img>
    <a href="https://trello.com/b/5gxtFXun/fluo"><img src="https://img.shields.io/badge/trello-here-000000FF?style=for-the-badge" height=24px></img></a>
</div>
<br>
<br>

> "Simplicity is the ultimate sophistication."
> <br>
> — Leonardo da Vinci

```rust
let fizzbuzz = (value: i32) {
    match loop 1..value {
        %% 15 -> print("Fizzbuzz")
        %% 3  -> print("Fizz")
        %% 5  -> print("Fizz")
        _     -> print(_)
    }
}

let entry = () {
    print("Hello, world!")
    fizzbuzz(100)
}
```

```rust
let fib = (n: i64) -> i64 {
    if n <= 1 {
        return 1
    }
    return fib(n - 1) + fib(n - 2)
}

let entry = () {
    print(fib(46))
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

3. Clone this repo (important: **clone recursively**):

   ```bash
   git clone --recurse-submodules -j8 git@github.com:fluo-lang/fluoc.git
   ```

4. There is no step 4

<br>

## Usage

Make sure you're in the fluo directory:

```bash
cd fluo
```

Fluo generates an output file `a.out`:

```bash
cargo run examples/tests.fl
./a.out
```

NOTE: you may also need to set `LLVM_SYS_100_PREFIX` to `/usr/lib/llvm-<version>` on Linux, or to `/usr/local/opt/llvm` on MacOS.

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

<br>

## More Examples

```rust
let make_func = (closure: () -> int) -> () -> int {
    return closure
}

let entry = () {
    let x: int = make_closure(() { return 10 })()

    -- Prints 10
    print(x)
}
```
