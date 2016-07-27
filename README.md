# table-layout

This package can be used to render character-based table layouts, which should be displayed with monospace fonts.

## Purpose

The focus of this library lies on rendering cells with different styles per column. Columns can be fixed in size or expanding to make content fit. Whenever content has to be cut, it is possible to indicate this with special strings (these are called cut marks). Columns can be positionally aligned as left, right or center and additionally aligned at certain character occurences, e.g. to display floating point numbers. Those specifications are then applied to a list of rows (currently only `String` is supported).

Typically cells are rendered as a grid, but it is also possible to render tables with simulated lines, including styling support. Such tables can use optional headers and multiple lines per cell. Multi-line content can be aligned vertically and text can be rendered justified.

## Tutorial

### Grid layout

Render some text rows as grid:
``` hs
putStrLn $ gridString [column expand left def def, column expand right def def]
                      [ ["top left", "top right"]
                      , ["bottom left", "bottom right"]
                      ]
```
`gridString` will join cells with a whitespace and rows with a newline character. The result is not spectacular but does look as expected:
```
top left       top right
bottom left bottom right
```
There are sensible default values for all column specification types, even for columns. We could have used just `def` for the first column.

### Number columns

Additionally some common types are provided. A particularly useful one is `numCol`:
``` hs
mapM_ putStrLn $ gridLines [numCol] (map ((: []) . show) [1.2, 100.5, 0.037, 5000.00001])
```
This will display the given numbers as a dot-aligned single column:
```
   1.2    
 100.5    
   3.7e-2 
5000.00001
```

### Improving readability of grids

Big grids are usually not that readable, so to improve their readability two functions are provided:

- `altLines` will alternate functions applied to lines.
- `checkeredCells` will checker cells with 2 different functions.

A good way to use this would be the [ansi-terminal package][], provided you are using a terminal to output your text.

### Table layout

Grids are fine, but sometimes we want to explicitly display a table, e.g. as output in a database application. This is where ```tableString``` comes in handy:

``` hs
putStrLn $ tableString [def , numCol]
                       unicodeRoundS
                       def
                       [ rowG ["Jack", "184.74"]
                       , rowG ["Jane", "162.2"]
                       ]
```
A row group is a group of rows which form one cell, meaning that each line of a group is not visually seperated from the other ones. In addition we specify the style and an optional header (which is not used by default). This will yield the following result:

```
╭──────┬────────╮
│ Jack │ 184.74 │
├──────┼────────┤
│ Jane │ 162.2  │
╰──────┴────────╯
```

### Table headers

Optionally we can use table headers. `titlesH` will center titles, whereas `fullH` allows more control:

``` hs
putStrLn $ tableString [fixedLeftCol 10, column (fixed 10) center dotAlign def]
                       unicodeS
                       (titlesH ["Text", "Number"])
                       [ rowG ["A very long text", "0.42000000"]
                       , rowG ["Short text", "100200.5"]
                       ]
```
Headers are always displayed with a different style, than the other columns (centered by default). A maximum column width is respected, otherwise a header may acquire additional space.
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
Because a row group consists of multiple lines, we may also want to align the content of cells vertically, especially when we don't know how many lines there will be. The following piece of code will display a left-justified text alongside the length of the text:
``` hs
let txt = "Lorem ipsum ..." 
in putStrLn $ tableString [fixedLeftCol 50, numCol]
                          asciiS
                          (titlesH ["Text", "Length"])
                          [colsAllG center [ justifyText 50 txt
                                           , [show $ length txt]
                                           ]
                          ]
```
`colsAllG` will merge the given columns into a row group with the given positioning:
```
+----------------------------------------------------+--------+
|                        Text                        | Length |
+----------------------------------------------------+--------+
| Lorem  ipsum dolor sit amet, consetetur sadipscing |        |
| elitr,  sed  diam nonumy eirmod tempor invidunt ut |        |
| labore  et  dolore  magna  aliquyam erat, sed diam |        |
| voluptua.  At  vero  eos  et  accusam et justo duo |    295 |
| dolores et ea rebum. Stet clita kasd gubergren, no |        |
| sea  takimata  sanctus  est  Lorem ipsum dolor sit |        |
| amet.                                              |        |
+----------------------------------------------------+--------+
```
Additionally, the positioning can be specified for each column with `colsG`. For grids `colsAsRows` and `colsAsRowsAll` are provided.

## Suggestions

Feel free to contact me, I'm always happy about some feedback!

[ansi-terminal package]: http://hackage.haskell.org/package/ansi-terminal
