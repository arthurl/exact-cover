# The `exact-cover` package

`exact-cover` is a Haskell library that implements a fast solver
for [exact cover problems](https://en.wikipedia.org/wiki/Exact_cover) using
Algorithm X as described in the
paper [_Dancing Links_](https://arxiv.org/abs/cs/0011047), by Donald Knuth, in
_Millennial Perspectives in Computer Science_, P159, 2000.

See [`exact-cover` on Hackage](https://hackage.haskell.org/package/exact-cover)
for more information.

## Examples

You will need to set the `buildExamples` flag to build the executable samples.
e.g. on stack, do

    stack build --flag exact-cover:buildExamples

### Sudoku

After setting the `buildExamples` flag above, the sudoku example can be run with

    stack exec sudoku

Interesting sudoku puzzles that one might use to try out the solver:

1. Unsolvable “puzzle” that
   took [Peter Norvig’s solver](http://norvig.com/sudoku.html) 1439 seconds to
   prove that there is no solution:

        .....5.8....6.1.43..........1.5........1.6...3.......553.....61........4.........

2. Finnish mathematician Arto Inkala’s
   [2006 puzzle](https://usatoday30.usatoday.com/news/offbeat/2006-11-06-sudoku_x.htm)

        85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.

    [2010 puzzle](http://www.mirror.co.uk/news/weird-news/worlds-hardest-sudoku-can-you-242294)

        ..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97..

3. A [compilation of resources](http://magictour.free.fr/sudoku.htm), which
   includes lists of “hardest sudokus”.
