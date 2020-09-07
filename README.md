# Lambda Spreadsheet

In this project, I have implemented a small language for computing on spreadsheets.

A spreadsheet (similar to an Excel sheet) is a dynamic data structure essentially made up cells, arranged in rows and columns, that can help store and process data.

Informally the grammar of spreadsheet formula language is:

- An instruction has a head and a body. The head consists of a cell indexing the output destination of the formula, and the body consists of an expression being evaluated with respect to the spreadsheet. The head and the body are separated by an assignment sign (:=)
- A formula ends with a semicolon (;)
- The arguments of a function are either indices, ranges, constants or expressions using mathematical operations
- An index is a pair of integers. General format for an index I of a cell is [i, j] Examples are [0,0], [3,4], …
- A range consists of two indices standing for the top left index and the right bottom index. General format for an range R of cells is ( I : I ) with an example being ( [ 5, 6] : [100, 6] )

In this spreadsheet, indexing will begin at index 0, and unlike Excel will be numeric for both rows and columns.

Formulas will be of 3 kinds: overall, row-wise, column-wise. The cell selection would be made by specifying the top-left and bottom-right cell indices. Results would be filled into the sheet by specifying only the top-left cell of the target cell/row/column.

The function operations can be one of the following:

Type 1 (unary operations on ranges of cells):

- COUNT
- ROWCOUNT
- COLCOUNT
- SUM
- ROWSUM
- COLSUM
- AVG
- ROWAVG
- COLAVG
- MIN
- ROWMIN
- COLMIN
- MAX
- ROWMAX
- COLMAX

Type 2 (binary operations on ranges of cells):

- ADD
- SUBT
- MULT
- DIV

Type 1 formula (unary operations on a range of cells)

- I := FUNC R ;

Type 2 formula (binary operations on ranges)

- I := FUNC R R ;
- I := FUNC C R ;
- I := FUNC R C ;
- I := FUNC I R ;
- I := FUNC R I ;

## Examples

Initial Sheet:
|     |  0  |  1  |  2  |
|-----|-----|-----|-----|
|**0**|  1  | 10  | 100 |
|**1**|  2  | 20  | 200 |
|**2**|  3  | 30  | 300 |

[0, 3] := ADD ( [0, 0] : [2, 0] ) ( [0, 1] : [2, 1] )

|     |  0  |  1  |  2  |  3  |
|-----|-----|-----|-----|-----|
|**0**|  1  | 10  | 100 | 11  |
|**1**|  2  | 20  | 200 | 22  |
|**2**|  3  | 30  | 300 | 33  |

[0, 4] := MULT ( [0, 0] : [2, 0] ) ( [0, 2] : [2, 2] )

|     |  0  |  1  |  2  |  3  |  4  |
|-----|-----|-----|-----|-----|-----|
|**0**|  1  | 10  | 100 | 11  | 100 |
|**1**|  2  | 20  | 200 | 22  | 400 |
|**2**|  3  | 30  | 300 | 33  | 900 |

[0, 5] := ADD ( [0, 4] : [2, 4]) 10.0

|     |  0  |  1  |  2  |  3  |  4  |  5  |
|-----|-----|-----|-----|-----|-----|-----|
|**0**|  1  | 10  | 100 | 11  | 100 | 110 |
|**1**|  2  | 20  | 200 | 22  | 400 | 410 |
|**2**|  3  | 30  | 300 | 33  | 900 | 910 |

[0, 3] := DIV ( [0, 4] : [2, 4] ) [1, 1]

|     |  0  |  1  |  2  |  3  |  4  |  5  |
|-----|-----|-----|-----|-----|-----|-----|
|**0**|  1  | 10  | 100 | 5   | 100 | 110 |
|**1**|  2  | 20  | 200 | 20  | 400 | 410 |
|**2**|  3  | 30  | 300 | 45  | 900 | 910 |

## Usage

### Compile

```bash
make
```

### Execute

```bash
./excel <csv_file_path> <m> <n> <formulas_file_path>
```

### Example

```bash
./excel example.csv 6 6 formulas.txt
```

### Clean

```bash
make clean
```
