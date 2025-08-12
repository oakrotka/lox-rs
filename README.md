# lox-rs
this is my rust implementation of an interpreter of the Lox language from the book
[Crafting Interpreters](https://craftinginterpreters.com/).
it mostly follows the book's implementation, but I took the freedom to pretty heavily reorganize
the code to better fit the code style of rust and my personal preferences.

## running
you can run the REPL using the command
```
cargo run -r
```
or you can evaluate a lox file with
```
cargo run -r -- code.lox
```

## testing
you can run the tests using the command
```
cargo test
```
