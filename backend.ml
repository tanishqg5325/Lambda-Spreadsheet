type index = INDICE of int * int
type range = RANGE of index * index
type value = FLOAT of float | UNDEFINED
(* A sheet can have both float as well as undefined values *)
type sheet = value list list

(* Unary and Binary Operator tokens *)
type tokens = COUNT | ROWCOUNT | COLCOUNT
            | SUM   | ROWSUM   | COLSUM
            | AVG   | ROWAVG   | COLAVG
            | MIN   | ROWMIN   | COLMIN
            | MAX   | ROWMAX   | COLMAX
            | ADD | SUBT | MULT | DIV

(* return type of parser *)
type formula = 
    UNARY of index * tokens * range
  | BINARY1 of index * tokens * range * range
  | BINARY2 of index * tokens * float * range
  | BINARY3 of index * tokens * index * range

(* Exceptions *)
exception NotPossible
exception InvalidRange
exception IncompatibleRange
exception EmptyCell
exception MaxSheetSizeReached

let maxSize = 5000;;

(* Function to check whether given range is valid or not *)
let isInvalidRange (r:range): bool = match r with
    RANGE(INDICE(i1, j1), INDICE(i2, j2)) -> (i1 > i2) || (j1 > j2);;

(* Function to check whether two ranges are compatible or not i.e. same number of rows and columns *)
let isIncompatibleRange (r1:range) (r2:range): bool = match r1 with 
    RANGE(INDICE(a1, a2), INDICE(b1, b2)) -> match r2 with
        RANGE(INDICE(c1, c2), INDICE(d1, d2)) -> ((b1-a1) <> (d1-c1)) || ((b2-a2) <> (d2-c2));;

(* Function to evaluate function f with identity e in range r in sheet s *)
let rec full_ans (s:sheet) (r:range) f (e:float): float =
  match s with
      [] -> (
        match r with RANGE(_, INDICE(i2, _)) ->
          if i2 < 0 then e else raise EmptyCell
      )
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then full_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
          else if i2 < 0 then e
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
              match v with
                  [] -> if i2 < 0 then e else raise EmptyCell
                | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                          else if i2 < 0 then e
                          else match y with
                              FLOAT(c) -> f c (full_row_ans ys 0 (i2-1))
                            | UNDEFINED -> raise EmptyCell
            in f (full_row_ans x j1 j2) (full_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e);;

(* Function to evaluate function f with identity e on every row of range r in sheet s *)
let rec row_ans (s:sheet) (r:range) f (e:float): float list =
  match s with
      [] -> (
        match r with RANGE(i, INDICE(i2, _)) ->
          if i2 < 0 then [] else raise EmptyCell
      )
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then row_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
          else if i2 < 0 then []
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
              match v with
                  [] -> if i2 < 0 then e else raise EmptyCell
                | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                          else if i2 < 0 then e
                          else match y with
                              FLOAT(c) -> f c (full_row_ans ys 0 (i2-1))
                            | UNDEFINED -> raise EmptyCell
            in (full_row_ans x j1 j2)::row_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e;;

(* Function to evaluate function f with identity e on every column of range r in sheet s *)
let rec col_ans (s:sheet) (r:range) f (e:float): float list =
  let rec mkzerov (n:int) (e:float): float list = if n = 0 then [] else e::mkzerov (n-1) e
  in match s with
        [] -> (
          match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
            if i2 < 0 then (mkzerov (j2-j1+1) e) else raise EmptyCell
        )
      | x::xs ->
          match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
            if i1 > 0 then col_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
            else if i2 < 0 then (mkzerov (j2-j1+1) e)
            else
              let rec merge (v1:value list) (v2:float list) (i1:int) (i2:int): float list =
                match v1 with
                    [] -> if i2 < 0 then [] else raise EmptyCell
                  | v::vs ->
                      if i1 > 0 then merge vs v2 (i1-1) (i2-1)
                      else if i2 < 0 then []
                      else match v with
                        FLOAT(c) -> (f c (List.hd v2))::merge vs (List.tl v2) 0 (i2-1)
                      | UNDEFINED -> raise EmptyCell
              in merge x (col_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e) j1 j2;;

(* Function to count the number of non-empty cells in range r in sheet s *)
let rec full_count_ans (s:sheet) (r:range): float =
  match s with
      [] -> 0.
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then full_count_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2)))
          else if i2 < 0 then 0.
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
                match v with 
                    [] -> 0.
                  | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                            else if i2 < 0 then 0.
                            else match y with
                                FLOAT(c) -> 1. +. (full_row_ans ys 0 (i2-1))
                              | UNDEFINED -> full_row_ans ys 0 (i2-1)
            in (full_row_ans x j1 j2) +. (full_count_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))));;

(* Function to count the number of non-empty cells in every row of range r in sheet s *)
let rec row_count_ans (s:sheet) (r:range): float list =
  let rec mkzerov (n:int): float list = if n = 0 then [] else 0.::mkzerov (n-1)
  in match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
    if i2 < 0 then []
    else match s with
        [] -> mkzerov (i2+1)
      | x::xs ->
          if i1 > 0 then row_count_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2)))
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
              match v with
                  [] -> 0.
                | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                          else if i2 < 0 then 0.
                          else match y with
                              FLOAT(c) -> 1. +. (full_row_ans ys 0 (i2-1))
                            | UNDEFINED -> full_row_ans ys 0 (i2-1)
            in (full_row_ans x j1 j2)::row_count_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2)));;

(* Function to count the number of non-empty cells in every column of range r in sheet s *)
let rec col_count_ans (s:sheet) (r:range): float list =
  let rec mkzerov (n:int): float list = if n = 0 then [] else 0.::mkzerov (n-1)
  in match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
    match s with
        [] -> mkzerov (j2-j1+1)
      | x::xs ->
          if i1 > 0 then col_count_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2)))
          else if i2 < 0 then mkzerov (j2-j1+1)
          else
            let rec merge (v1:value list) (v2:float list) (i1:int) (i2:int): float list =
              match v1 with
                  [] -> v2
                | v::vs ->
                    if i1 > 0 then merge vs v2 (i1-1) (i2-1)
                    else if i2 < 0 then []
                    else match v with
                      FLOAT(c) -> (1. +. (List.hd v2))::merge vs (List.tl v2) 0 (i2-1)
                    | UNDEFINED -> (List.hd v2)::merge vs (List.tl v2) 0 (i2-1)
            in merge x (col_count_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2)))) j1 j2;;

(* Function to evaluate function f with one parameter as c on range r in sheet s *)
let rec full_range_ans (s:sheet) (r:range) (c:float) f: float list list =
  match s with
      [] -> (
        match r with RANGE(i, INDICE(i2, _)) ->
          if i2 < 0 then [] else raise EmptyCell
      )
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then full_range_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) c f
          else if i2 < 0 then []
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float list =
                match v with
                    [] -> if i2 < 0 then [] else raise EmptyCell
                  | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                            else if i2 < 0 then []
                            else match y with
                                FLOAT(x) -> (f x c)::(full_row_ans ys 0 (i2-1))
                              | UNDEFINED -> raise EmptyCell
            in (full_row_ans x j1 j2)::(full_range_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) c f);;

(* Function to expand sheet s to dimensions h X l filling new cells with UNDEFINED *)
let rec expandSheet (s:sheet) (h:int) (l:int): sheet =
  let rec makeUndefinedRow (n:int): value list = if n = 0 then [] else UNDEFINED::makeUndefinedRow (n-1)
  in match s with
        [] -> if h = 0 then s else (makeUndefinedRow l)::expandSheet s (h-1) l
      | x::xs -> if l = (List.length x) then x::expandSheet xs (h-1) l
                 else (x @ (makeUndefinedRow (l-(List.length x))))::expandSheet xs (h-1) l;;

(* Function to write float c in index i_ of sheet s *)
let rec writeCell (s:sheet) (i_:index) (c:float): sheet =
  match s with
      [] -> []
    | x::xs ->
        match i_ with INDICE(i, j) ->
          if i > 0 then x::writeCell xs (INDICE(i-1, j)) c
          else
            let rec colWriteCell (v:value list) (j:int): value list =
              if j = 0 then FLOAT(c)::(List.tl v)
              else (List.hd v)::colWriteCell (List.tl v) (j-1)
            in (colWriteCell x j)::xs;;

(* Function to write row c in sheet s starting with index i_ *)
let rec writeRow (s:sheet) (i_:index) (c:float list): sheet =
  match s with
      [] -> []
    | x::xs ->
        match i_ with INDICE(i, j) ->
          if i > 0 then x::writeRow xs (INDICE(i-1, j)) c
          else
            let rec writeRowUtil (v:value list) (j:int) (c:float list): value list =
              if j > 0 then (List.hd v)::writeRowUtil (List.tl v) (j-1) c
              else if (List.length c) = 0 then v
              else FLOAT((List.hd c))::writeRowUtil (List.tl v) 0 (List.tl c)
            in (writeRowUtil x j c)::xs;;

(* Function to write column c in sheet s starting with index i_ *)
let rec writeCol (s:sheet) (i_:index) (c:float list): sheet =
  match s with
      [] -> []
    | x::xs ->
        match i_ with INDICE(i, j) ->
          if i > 0 then x::writeCol xs (INDICE(i-1, j)) c
          else if (List.length c) = 0 then s
          else
            let rec colWriteCell (v:value list) (j:int) (c:float): value list =
              if j = 0 then FLOAT(c)::(List.tl v)
              else (List.hd v)::colWriteCell (List.tl v) (j-1) c
            in (colWriteCell x j (List.hd c))::writeCol xs i_ (List.tl c);;

(* Function to write range c in sheet s starting with index i_ *)
let rec writeRange (s:sheet) (i_:index) (c:float list list): sheet =
  match s with
      [] -> []
    | x::xs ->
        match i_ with INDICE(i, j) ->
          if i > 0 then x::writeRange xs (INDICE(i-1, j)) c
          else if (List.length c) = 0 then s
          else
            let rec writeRowUtil (v:value list) (j:int) (c:float list): value list =
              if j > 0 then (List.hd v)::writeRowUtil (List.tl v) (j-1) c
              else if (List.length c) = 0 then v
              else FLOAT((List.hd c))::writeRowUtil (List.tl v) 0 (List.tl c)
            in (writeRowUtil x j (List.hd c))::writeRange xs i_ (List.tl c);;

(* Helper function for unary operators of overall kind *)
let unary_full (s:sheet) (i_:index) (ans:float): sheet =
  match i_ with INDICE(i, j) ->
    if i >= List.length s || j >= List.length (List.hd s) then
      let new_h = max (i+1) (List.length s) in
      let new_l = max (j+1) (List.length (List.hd s)) in
      if new_h > maxSize || new_l > maxSize then raise MaxSheetSizeReached
      else writeCell (expandSheet s new_h new_l) i_ ans
    else writeCell s i_ ans;;

(* Helper function for unary operators of row-wise kind *)
let unary_row (s:sheet) (r:range) (i_:index) (ans:float list): sheet =
  match i_ with INDICE(i, j) ->
    match r with RANGE(INDICE(i1, _), INDICE(i2, _)) ->
    if i+i2-i1 >= List.length s || j >= List.length (List.hd s) then
      let new_h = max (i+i2-i1+1) (List.length s) in
      let new_l = max (j+1) (List.length (List.hd s)) in
      if new_h > maxSize || new_l > maxSize then raise MaxSheetSizeReached
      else writeCol (expandSheet s new_h new_l) i_ ans
    else writeCol s i_ ans;;

(* Helper function for unary operators of column-wise kind *)
let unary_col (s:sheet) (r:range) (i_:index) (ans: float list):sheet =
  match i_ with INDICE(i, j) ->
    match r with RANGE(INDICE(_, j1), INDICE(_, j2)) ->
    if i >= List.length s || j+j2-j1 >= List.length (List.hd s) then
      let new_h = max (i+1) (List.length s) in
      let new_l = max (j+j2-j1+1) (List.length (List.hd s)) in
      if new_h > maxSize || new_l > maxSize then raise MaxSheetSizeReached
      else writeRow (expandSheet s new_h new_l) i_ ans
    else writeRow s i_ ans;;

(* Fills count of valid entries in the given range into the specified cell *)
let full_count (s:sheet) (r:range) (i_:index): sheet =
  let ans = full_count_ans s r in unary_full s i_ ans;;

(* Fills count of valid entries per row in the given range into the column starting from the specified cell *)
let row_count (s:sheet) (r:range) (i_:index): sheet =
  let ans = row_count_ans s r in unary_row s r i_ ans;;

(* Fills count of valid entries per column in the given range into the row starting from the specified cell *)
let col_count (s:sheet) (r:range) (i_:index): sheet =
  let ans = col_count_ans s r in unary_col s r i_ ans;;

(* Fills the sum of entries of cells in the given range into the specified cell *)
let full_sum (s:sheet) (r:range) (i_:index): sheet = 
  let f a b = a +. b in
  let ans = full_ans s r f 0. in unary_full s i_ ans;;

(* Fills the sum of entries of cells per row in the given range into the column starting from the specified cell *)
let row_sum (s:sheet) (r:range) (i_:index): sheet =
  let f a b = a +. b in 
  let ans = row_ans s r f 0. in unary_row s r i_ ans;;

(* Fills the sum of entries of cells per column in the given range into the row starting from the specified cell *)
let col_sum (s:sheet) (r:range) (i_:index): sheet =
  let f a b = a +. b in 
  let ans = col_ans s r f 0. in unary_col s r i_ ans;;

(* Fills the average of entries of cells in the given range into the specified cell *)
let full_avg (s:sheet) (r:range) (i_:index): sheet =
  match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
  let f a b = a +. b in
  let ans = (full_ans s r f 0.) /. ((float_of_int (i2-i1+1)) *. (float_of_int (j2-j1+1))) in
  unary_full s i_ ans;;

let rec divideList (v:float list) (c:float): float list = match v with
    [] -> []
  | x::xs -> (x /. c)::divideList xs c

(* Fills the average of entries of cells per row in the given range into the column starting from the specified cell *)
let row_avg (s:sheet) (r:range) (i_:index): sheet =
  match r with RANGE(INDICE(_, j1), INDICE(_, j2)) ->
  let f a b = a +. b in
  let ans = divideList (row_ans s r f 0.) (float_of_int (j2-j1+1)) in
  unary_row s r i_ ans;;

(* Fills the sum of entries of cells per column in the given range into the row starting from the specified cell *)
let col_avg (s:sheet) (r:range) (i_:index): sheet =
  match r with RANGE(INDICE(i1, _), INDICE(i2, _)) ->
  let f a b = a +. b in
  let ans = divideList (col_ans s r f 0.) (float_of_int (i2-i1+1)) in
  unary_col s r i_ ans;;

(* Fills the min of entries of cells in the given range into the specified cell *)
let full_min (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (min a b) in
  let ans = full_ans s r f max_float in unary_full s i_ ans;;

(* Fills the min of entries of cells per row in the given range into the column starting from the specified cell *)
let row_min (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (min a b) in 
  let ans = row_ans s r f max_float in unary_row s r i_ ans;;

(* Fills the min of entries of cells per column in the given range into the row starting from the specified cell *)
let col_min (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (min a b) in 
  let ans = col_ans s r f max_float in unary_col s r i_ ans;;

(* Fills the max of entries of cells in the given range into the specified cell *)
let full_max (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (max a b) in
  let ans = full_ans s r f min_float in unary_full s i_ ans;;

(* Fills the max of entries of cells per row in the given range into the column starting from the specified cell *)
let row_max (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (max a b) in 
  let ans = row_ans s r f min_float in unary_row s r i_ ans;;

(* Fills the max of entries of cells per column in the given range into the row starting from the specified cell *)
let col_max (s:sheet) (r:range) (i_:index): sheet =
  let f a b = (max a b) in 
  let ans = col_ans s r f min_float in unary_col s r i_ ans;;

(* applies the function f to the contents of each cell in the selected cell range *)
let binary_range_const (s:sheet) (r:range) (c:float) (i_:index) f: sheet =
  let ans = full_range_ans s r c f in
  match i_ with INDICE(i, j) ->
    match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
    if i+i2-i1 >= List.length s || j+j2-j1 >= List.length (List.hd s) then
      let new_h = max (i+i2-i1+1) (List.length s) in
      let new_l = max (j+j2-j1+1) (List.length (List.hd s)) in
      if new_h > maxSize || new_l > maxSize then raise MaxSheetSizeReached
      else writeRange (expandSheet s new_h new_l) i_ ans
    else writeRange s i_ ans;;

(* adds a constant to the contents of each cell in the selected cell range *)
let add_const (s:sheet) (r:range) (c:float) (i_:index): sheet =
  let f a b = a +. b in binary_range_const s r c i_ f;;

(* subtracts a constant from the contents of each cell in the selected cell range *)
let subt_const (s:sheet) (r:range) (c:float) (i_:index): sheet =
  let f a b = a -. b in binary_range_const s r c i_ f;;

(* multiplies the contents of each cell in the selected cell range by a constant *)
let mult_const (s:sheet) (r:range) (c:float) (i_:index): sheet =
  let f a b = a *. b in binary_range_const s r c i_ f;;

(* divides the contents of each cell in the selected cell range by a constant *)
let div_const (s:sheet) (r:range) (c:float) (i_:index): sheet =
  let f a b = a /. b in binary_range_const s r c i_ f;;

(* applies the function f element wise on two matrices c1 and c2 *)
let rec applyOperation (c1:float list list) (c2:float list list) f: float list list =
  match c1 with
      [] -> []
    | x::xs ->
        let rec rowUtil (c1:float list) (c2:float list): float list =
          match c1 with
              [] -> []
            | y::ys -> (f y (List.hd c2))::rowUtil ys (List.tl c2)
        in (rowUtil x (List.hd c2))::applyOperation xs (List.tl c2) f;;

(* performs the function f on the cell contents for each corresponding pair of cells in two selected cell ranges *)
let binary_range_range (s:sheet) (r1:range) (r2:range) (i_:index) f: sheet =
  let g a b = a +. b in
  let ans = applyOperation (full_range_ans s r1 0. g) (full_range_ans s r2 0. g) f in
  match i_ with INDICE(i, j) ->
    match r1 with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
    if i+i2-i1 >= List.length s || j+j2-j1 >= List.length (List.hd s) then
      let new_h = max (i+i2-i1+1) (List.length s) in
      let new_l = max (j+j2-j1+1) (List.length (List.hd s)) in
      if new_h > maxSize || new_l > maxSize then raise MaxSheetSizeReached
      else writeRange (expandSheet s new_h new_l) i_ ans
    else writeRange s i_ ans;;

(* adds the cell contents for each corresponding pair of cells in two selected cell ranges *)
let add_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet =
  let f a b = a +. b in binary_range_range s r1 r2 i_ f;;

(* performs a subtraction on the cell contents for each corresponding pair of cells in two selected cell ranges *)
let subt_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet =
  let f a b = a -. b in binary_range_range s r1 r2 i_ f;;

(* multiplies the cell contents for each corresponding pair of cells in two selected cell ranges *)
let mult_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet =
  let f a b = a *. b in binary_range_range s r1 r2 i_ f;;

(* performs a division on the cell contents for each corresponding pair of cells in two selected cell ranges *)
let div_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet =
  let f a b = a /. b in binary_range_range s r1 r2 i_ f;;

(* Function which returns the value at given index in given sheet *)
let rec getValueAtIndex (s:sheet) (i:index): value =
    match s with
        [] -> UNDEFINED
      | x::xs -> match x with
                    [] -> UNDEFINED
                  | y::ys ->  match i with INDICE(i, j) ->
                                if(i = 0 && j = 0) then y
                                else if(i = 0) then getValueAtIndex (ys::xs) (INDICE(i, j-1))
                                else getValueAtIndex xs (INDICE(i-1, j));;

(* Interpreter based on formulas returned by parser *)
let eval (s:sheet) (f:formula): sheet = match f with
    UNARY(i_, t, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match t with
          COUNT     -> full_count s r i_
        | ROWCOUNT  -> row_count s r i_
        | COLCOUNT  -> col_count s r i_
        | SUM       -> full_sum s r i_
        | ROWSUM    -> row_sum s r i_
        | COLSUM    -> col_sum s r i_
        | AVG       -> full_avg s r i_
        | ROWAVG    -> row_avg s r i_
        | COLAVG    -> col_avg s r i_
        | MIN       -> full_min s r i_
        | ROWMIN    -> row_min s r i_
        | COLMIN    -> col_min s r i_
        | MAX       -> full_max s r i_
        | ROWMAX    -> row_max s r i_
        | COLMAX    -> col_max s r i_
        | _         -> raise NotPossible
    )

  | BINARY1(i_, t, r1, r2) -> (
      if (isInvalidRange r1) || (isInvalidRange r2) then raise InvalidRange
      else if (isIncompatibleRange r1 r2) then raise IncompatibleRange
      else match t with
          ADD   -> add_range s r1 r2 i_
        | SUBT  -> subt_range s r1 r2 i_
        | MULT  -> mult_range s r1 r2 i_
        | DIV   -> div_range s r1 r2 i_
        | _     -> raise NotPossible
    )

  | BINARY2(i_, t, c, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match t with
          ADD   -> add_const s r c i_
        | SUBT  -> subt_const s r c i_
        | MULT  -> mult_const s r c i_
        | DIV   -> div_const s r c i_
        | _     -> raise NotPossible
    )

  | BINARY3(i_, t, i, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match (getValueAtIndex s i) with
          UNDEFINED -> raise EmptyCell
        | FLOAT(c)  -> match t with
                          ADD   -> add_const s r c i_
                        | SUBT  -> subt_const s r c i_
                        | MULT  -> mult_const s r c i_
                        | DIV   -> div_const s r c i_
                        | _     -> raise NotPossible
    )
;;
