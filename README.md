# table-layout

This package can be used to render character-based table layouts, which should be displayed with monospace fonts.

## Purpose

The focus of this library lies on rendering cells with different styles per column. Columns can be fixed in size or expanding to make content fit. Whenever content has to be cut, it is possible to indicate this with special strings (these are called cut marks). Columns can be positionally aligned as left, right or center and additionally aligned at certain character occurences, e.g. to display floating point numbers. Those specifications are then applied to a list of rows (currently only String is supported).

Typically cells are rendered as a grid, but it is also possible to render tables with simulated lines, including styling support. Such tables can use optional headers and multiple lines per cell. Multi-line content can be aligned vertically and text can be rendered justified.

## Examples

Render some text rows as grid:
``` hs
putStr $ layoutToString [ ["top left", "top right"]
                        , ["bottom left", "bottom right"]
                        ]
                        [ column expand left noAlign def
                        , column expand right noAlign def
                        ]
```
Result:
```
top left       top right
bottom left bottom right
```
There are sensible default values for all column specification types, even for columns. Additionally some common types are provided. A particularly useful one is `numCol`:
``` hs
putStr $ layoutToString (map ((: []) . show) [1.2, 100.5, 0.037, 5000.00001]) [numCol]
```
Result:
```
   1.2    
 100.5    
   3.7e-2 
5000.00001
```





