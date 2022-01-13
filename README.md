# What is this?

This is a SAT solver that takes in input constraints expressed using the language in [formula.ml](formula.ml) and reduces them to a Reduced Ordered Binary Decision Diagram (ROBDD). It then uses the generated ROBDD to identify satsifying assignments and can also output the ROBDD as a graph in GraphViz dot (`.dot`) format. 

For an example graph, see [`nqueens4.png`](nqueens4.png) for an image of the ROBDD for the 4-Queens problem generated from [`nqueens4.dot`](nqueens4.dot). There is also a similar example graph for the 5-Queens problem - [`nqueens5.png`](nqueens5.png), [`nqueens5.dot`](nqueens5.dot). The graph contains variables of the form `cij` where `i` and `j` are numbers and this variable represents the existence of a queen on row `i` and column `j` of the board. A satisfying assignment to these variables is given by any path that leads from the root of the ROBDD to the value 1 at the bottom of the ROBDD and the assignment itself for a variable is given by whether the edge taken from that variable in the chosen path is 1 (true) or 0 (false).


# File Structure
- `formula.ml`: Basic structure of formulas accepted by solver.
- `bdd.mli`: Interface of bdd solver module.
- `bdd.ml`: Implementation of bdd solver module.
- `A2.ml`: The functions in this file uses the BDD module to solve the n_queens problem.

# Compilation and Execution
You can compile the whole project as 
```
ocamlc formula.ml bdd.mli bdd.ml A2.ml
```

To run the package on an n-queen problem:
- Append `let ro = n_queen <N>;;` to the end of A2.ml, where `<N>` is replaced with the board-size for which you need a satisfying solution 
- Compile using the above command
- Execute the `a.out` file generated

The package would generate a `bdd.dot` file encoding the ROBDD for the corresponding N-Queens problem.