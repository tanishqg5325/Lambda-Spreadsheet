open Lexer;;
open Parser;;
open Backend;;


let applyFormula (t:sheet) (s:string): sheet =
  eval t (Parser.main Lexer.read (Lexing.from_string s));;

let is_float s =
  try ignore (float_of_string s); true
  with _ -> false
;;

let rec convertListToSheet l = match l with
    [] -> []
  | x::xs -> if is_float x then FLOAT((float_of_string x))::convertListToSheet xs
            else UNDEFINED::convertListToSheet xs
;;

let rec read_file in_stream =
  try
    let line = input_line in_stream in
    let split = Str.split (Str.regexp ",") in
    (convertListToSheet (split line))::read_file in_stream
  with End_of_file -> []
;;

let in_stream = open_in Sys.argv.(1);;
let s0 = read_file in_stream;;

let rec getWidthOfSheet s = match s with
    [] -> 0
  | x::xs -> max (List.length x) (getWidthOfSheet xs)
;;

let m = max (int_of_string Sys.argv.(2)) (List.length s0);;
let n = max (int_of_string Sys.argv.(3)) (getWidthOfSheet s0);;
if m > 5000 || n > 5000 then raise MaxSheetSizeReached;;
let s = expandSheet s0 m n;;

let rec read_formulas in_stream =
  try
    let line = input_line in_stream in line::read_formulas in_stream
  with End_of_file -> []
;;

let rec print_list = function 
  [] -> ()
  | e::l -> match e with
        FLOAT(c) -> print_float c ; print_string "\t"; print_list l
      | UNDEFINED -> print_string "E\t" ; print_list l
;;

let rec print_sheet = function
    [] -> ()
  | x::xs -> print_list x; print_string "\n"; print_sheet xs
;;

let formula_stream = open_in Sys.argv.(4);;
let formulas_list = read_formulas formula_stream;;

let rec processFormulas (s:sheet) (f:string list) = match f with
    [] -> ()
  | x::xs -> let s' = applyFormula s x in
             print_sheet s'; print_string "\n"; processFormulas s' xs
;;

print_sheet s;; print_string "\n";;
processFormulas s formulas_list;;
