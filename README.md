# Sequent solver
This is a simple sequent solver, implemented in rust.
 
 Usage:
  * `cargo run` to get cmd-line interface. Expressions must be written as: `F, T => G, D`. Syntax:
    * `-F` logical not
    * `F | G` logical or
    * `F & G` logical and
    * `F > G` logical implication
    * `0`,`1` for true and false
  * `cargo test` to run all tests.