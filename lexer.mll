(* Header *)
{
    (* import tokens from parser rather than defining again *)
    (* Added a token for eof *)
    open Parser;;
    exception InvalidToken of char ;;
}


let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* Regex for natural numbers *)
let number = ('0' | ((digit_)digit*))
(* Regex for floating constants *)
let float_constant = ['+''-']?(number)['.']('0'|digit*(digit_))
(* Regex for whitespace *)
let sp = [' ' '\t' '\n']+       (* Added newline *)

(* removed "read lexbuf" as we don't require token list now, parser does this itself *)
rule read = parse
      eof                       {EOF}                       (* Added end of file *)
    | sp                        {read lexbuf}
    | float_constant as f       {FLOAT(float_of_string f)}
    | '('                       {LP}
    | ')'                       {RP}
    | '['                       {LB}
    | ']'                       {RB}
    | ','                       {COMMA}
    | ':'                       {COLON}
    | ['[']sp*(number as n1)sp*[',']sp*(number as n2)sp*[']'] (* Regex for indice *)
        {INDICE((int_of_string n1), (int_of_string n2))}
    | ['(']sp*['[']sp*(number as n1)sp*[',']sp*(number as n2)sp*[']']sp*[':']sp*['[']sp*(number as n3)sp*[',']sp*(number as n4)sp*[']']sp*[')'] (* Regex for range *)
        {RANGE(INDICE((int_of_string n1), (int_of_string n2)), INDICE((int_of_string n3), (int_of_string n4)))}
    | "COUNT"                   {COUNT}
    | "ROWCOUNT"                {ROWCOUNT}
    | "COLCOUNT"                {COLCOUNT}
    | "SUM"                     {SUM}
    | "ROWSUM"                  {ROWSUM}
    | "COLSUM"                  {COLSUM}
    | "AVG"                     {AVG}
    | "ROWAVG"                  {ROWAVG}
    | "COLAVG"                  {COLAVG}
    | "MIN"                     {MIN}
    | "ROWMIN"                  {ROWMIN}
    | "COLMIN"                  {COLMIN}
    | "MAX"                     {MAX}
    | "ROWMAX"                  {ROWMAX}
    | "COLMAX"                  {COLMAX}
    | "ADD"                     {ADD}
    | "SUBT"                    {SUBT}
    | "MULT"                    {MULT}
    | "DIV"                     {DIV}
    | ":="                      {EQ}
    | ';'                       {DELIMITER}
    | _ as s                    {raise (InvalidToken s)} (* An exception is raised if any string apart from expected strings are encountered *)
