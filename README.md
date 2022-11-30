# table-layout [![Hackage version](https://img.shields.io/hackage/v/table-layout.svg?label=Hackage)](https://hackage.haskell.org/package/table-layout)

This package can be used to render character-based table layouts which should be displayed with monospace fonts.

## Purpose

The focus of this library lies on rendering cells with different styles per column:
* Columns can be fixed in size or expanding to make content fit.
* Whenever content has to be cut, it is possible to indicate this with special strings, which are called cut marks.
* Columns can be positionally aligned as `left`, `right` or `center`.
* Columns may also be aligned at certain character occurence with respect to the other cells of that column. One such purpose is to display floating point numbers.

Those specifications are then applied to a list of rows. A row is simply a list of a cell. A cell is a type that implements the `Cell` type class.

Typically cells are rendered as a grid, but it is also possible to render tables with simulated lines, including styling support. Such tables can use optional headers and multiple lines per cell. Multi-line content can be aligned vertically, with respect to the other horizontally adjacent cells, and text can be rendered justified.

## Tutorial

### Grids

Render some text rows as grid:
``` hs
> let g = [["top left", "top right"], ["bottom left", "bottom right"]]
> putStrLn $ gridString [column expand left def def, column expand right def def] g
```
`gridString` will join cells with a whitespace and rows with a newline character. The result is not spectacular but does look as expected:
```
top left       top right
bottom left bottom right
```
There are sensible default values for all column specification types, even for columns. We could have used just `def` for the first column.

### Number Columns

Additionally some common types are provided. A particularly useful one is `numCol`:
``` hs
> import Numeric
> let toRow d = [showFFloat Nothing d ""]
> mapM_ putStrLn $ gridLines [numCol] $ toRow <$> [1.2, 100.5, 0.037, 5000.00001]
```
This will display the given numbers as a dot-aligned single column:
```
   1.2    
 100.5    
   0.037
5000.00001
```

### Improving Readability of Grids

Big grids are usually not that readable. To improve their readability, two functions are provided:

* `altLines` will apply the given function in an alternating pattern. E.g., color every second row grey.
* `checkeredCells` will checker cells with 2 different functions.

A good way to use this would be the [ansi-terminal package][], provided you are using a terminal to output your text. Another way to introduce color into cells is the `Formatted` type:
```
> :set -XOverloadedStrings
> import Text.Layout.Table.Cell.Formatted
> import System.Console.ANSI.Codes
> let red s = formatted (setSGRCode [SetColor Foreground Dull Red]) (plain s) (setSGRCode [Reset])
> let g = [["Jim", "1203"], ["Jane", "523"], ["Jack", red "-959000"]]
> putStrLn $ gridString [def, numCol] g
```
This way the color can depend on the cell content.

### Tables

For more complex data, grids do not offer as much visibility. Sometimes we want to explicitly display a table, for example, as output in a database application. `tableLines` and `tableString` are used to create a table.

``` hs
> let t = headerlessTableS [def , numCol] unicodeRoundS [rowG ["Jack", "184.74"], rowG ["Jane", "162.2"]]
> putStrLn $ tableString t
```
A row group is a group of rows which are not visually separated from each other. Thus multiple rows form one cell.

In addition we specify the style and an optional header. By default the header is not visible. This will yield the following result:
```
╭──────┬────────╮
│ Jack │ 184.74 │
├──────┼────────┤
│ Jane │ 162.2  │
╰──────┴────────╯
```

### Table Headers

Optionally we can use table headers. `titlesH` will center titles, whereas `fullH` allows more control:

``` hs
> let cs = [fixedLeftCol 10, column (fixed 10) center dotAlign def]
> let h = (titlesH ["Text", "Number"])
> let rgs = [rowG ["A very long text", "0.42000000"], rowG ["Short text", "100200.5"]]
> let t = columnHeaderTableS cs unicodeS h rgs
> putStrLn $ tableString t
```
Headers are always displayed with a different style than the other columns (centered by default). A maximum column width is respected, otherwise a header may acquire additional space.
```
┌────────────┬────────────┐
│    Text    │   Number   │
╞════════════╪════════════╡
│ A very lo… │   0.42000… │
├────────────┼────────────┤
│ Short text │ …00.5      │
└────────────┴────────────┘
```
### Vertical positioning and justified text
Because a row group consists of multiple lines, we may also want to align the content of cells vertically, especially when we do not know how many lines there will be.  The following piece of code will display a left-justified text alongside the length of the text:
``` hs
> let txt = "Lorem ipsum ..." 
> let rgs = [colsAllG center [justifyText 50 txt, [show $ length txt]]]
> let cs = [fixedLeftCol 50, numCol]
> let h = titlesH ["Text", "Length"]
> let t = columnHeaderTableS cs asciiS h rgs
> putStrLn $ tableString t
```
`colsAllG` will merge the given columns into a row group with the given positioning:
```
+----------------------------------------------------+--------+
|                        Text                        | Length |
+----------------------------------------------------+--------+
| Lorem  ipsum dolor sit amet, consectetur adipisici |        |
| elit,  sed eiusmod  tempor incidunt  ut labore  et |        |
| dolore magna aliqua. Ut enim ad minim veniam, quis |        |
| nostrud   exercitation  ullamco  laboris  nisi  ut |        |
| aliquid  ex ea  commodi consequat.  Quis aute iure |    429 |
| reprehenderit   in  voluptate  velit  esse  cillum |        |
| dolore  eu fugiat  nulla pariatur.  Excepteur sint |        |
| obcaecat cupiditat non proident, sunt in culpa qui |        |
| officia deserunt mollit anim id est laborum.       |        |
+----------------------------------------------------+--------+
```
Additionally, the positioning can be specified for each column with `colsG`.  For grids `colsAsRows` and `colsAsRowsAll` are provided.

## Contact

* Please report issues and suggestions to the GitHub page.
* Any kind of feedback is welcome.
* Contributions are much appreciated. Contact me first for bigger changes.

[ansi-terminal package]: http://hackage.haskell.org/package/ansi-terminal
