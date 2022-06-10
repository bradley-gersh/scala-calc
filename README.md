# ScalaCalc

A simple calculator program on the command line, used as a learning exercise for Scala 3.

## Build

From the root directory of the repository, run

```shell
sbt compile
```

To execute the program, run

```shell
sbt run
```

## Usage

The calculator runs as a read-eval-print loop. Enter a blank line to exit.

At the prompt, enter an arithmetic expression using integers or decimal numbers, parentheses, and the usual operators
`+ - * /` (infix notation) to evaluate it. Expressions can be nested, and standard order of operations applies:

```
> 3 + 5.5
= 8.5

> 4 * 4 - 6
= 10.0

> 4 * (4 - 6)            
= -8.0

> (2 + 3) * ((3 - 4) / (0.5 * 2))
= -5.0
```

Prepend the expression with `?` to view a flat representation of the parse tree in prefix notation (credit to [Bob
Nystrom](http://craftinginterpreters.com/representing-code.html) for the idea):

```
> ?3 + 5.5
--> parse tree: (+ 3.0 5.5)
= 8.5

> ?4 * 4 - 6
--> parse tree: (- (* 4.0 4.0) 6.0)
= 10.0

> ?4 * (4 - 6)
--> parse tree: (* 4.0 (- 4.0 6.0))
= -8.0

> ?(2 + 3) * ((3 - 4) / (0.5 * 2))
--> parse tree: (* (+ 2.0 3.0) (/ (- 3.0 4.0) (* 0.5 2.0)))
= -5.0
```

## References

This project was heavily inspired by chapters 4–7 of Bob Nystrom's book [*Crafting Interpreters*](https://craftinginterpreters.com),
with some reference to Matt Might's [Scheme parser](https://matt.might.net/articles/parsing-s-expressions-scala/) in Scala.