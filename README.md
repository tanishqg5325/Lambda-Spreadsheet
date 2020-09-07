# Lambda Spreadsheet

In this project, I have implemented a small language for computing on spreadsheets.

A spreadsheet (similar to an Excel sheet) is a dynamic data structure essentially made up cells, arranged in rows and columns, that can help store and process data.

Informally the grammar of spreadsheet formula language is:

- An instruction has a head and a body. The head consists of a cell indexing the output destination of the formula, and the body consists of an expression being evaluated with respect to the spreadsheet. The head and the body are separated by an assignment sign (:=)
- A formula ends with a semicolon (;)
- The arguments of a function are either indices, ranges, constants or expressions using mathematical operations
- An index is a pair of integers. General format for an index I of a cell is [i, j] Examples are [0,0], [3,4], …
- A range consists of two indices standing for the top left index and the right bottom index. General format for an range R of cells is ( I : I ) with an example being ( [ 5, 6] : [100, 6] )

## Usage

Compile:

```bash
make
```

Execute:

```bash
./excel <csv_file_path> <m> <n> <formulas_file_path>
```

Example:

```bash
./excel example.csv 6 6 formulas.txt
```

Clean:

```bash
make clean
```
