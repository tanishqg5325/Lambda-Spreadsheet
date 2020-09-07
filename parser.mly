/*
    PARSER
    - For making sense of info passed by lexer using a defined grammar
    - Based on a basic CFG
    - Returns the formula by parsing
*/

%{
    open Backend;;
%}

/* DEFINING TOKENS. To be used in lexer */

/* floating constants */
%token <float> FLOAT
/* index */
%token <int * int> INDICE
/* range */
%token <Backend.index * Backend.index> RANGE
/* other tokens */
%token LP RP LB RB COMMA COLON COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM AVG ROWAVG COLAVG MIN ROWMIN COLMIN MAX ROWMAX COLMAX ADD SUBT MULT DIV EQ DELIMITER EOF

/* STARTING GRAMMAR */
/* Defining the start symbol for the grammar */
%start main
/* Referring to the return type as 'Backend.formula' */
%type <Backend.formula> main
%%

main:
  /* Main */
  formulas DELIMITER EOF              {$1}
;

formulas:
    /* I := FUNC R ; */
    INDICE EQ unary RANGE           {UNARY(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)))}
    /* I := FUNC R R ; */
  | INDICE EQ binary RANGE RANGE    {BINARY1(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)), RANGE((fst $5),(snd $5)))}
    /* I := FUNC C R ; */
  | INDICE EQ binary FLOAT RANGE    {BINARY2(INDICE((fst $1),(snd $1)), $3, $4, RANGE((fst $5),(snd $5)))}
    /* I := FUNC R C ; */
  | INDICE EQ binary RANGE FLOAT    {BINARY2(INDICE((fst $1),(snd $1)), $3, $5, RANGE((fst $4),(snd $4)))}
    /* I := FUNC I R ; */
  | INDICE EQ binary INDICE RANGE   {BINARY3(INDICE((fst $1),(snd $1)), $3, INDICE((fst $4),(snd $4)), RANGE((fst $5),(snd $5)))}
    /* I := FUNC R I ; */
  | INDICE EQ binary RANGE INDICE   {BINARY3(INDICE((fst $1),(snd $1)), $3, INDICE((fst $5),(snd $5)), RANGE((fst $4),(snd $4)))}
;

unary:
    /* Unary Operators */
    COUNT         {COUNT}
  | ROWCOUNT      {ROWCOUNT}
  | COLCOUNT      {COLCOUNT}
  | SUM           {SUM}
  | ROWSUM        {ROWSUM}
  | COLSUM        {COLSUM}
  | AVG           {AVG}
  | ROWAVG        {ROWAVG}
  | COLAVG        {COLAVG}
  | MIN           {MIN}
  | ROWMIN        {ROWMIN}
  | COLMIN        {COLMIN}
  | MAX           {MAX}
  | ROWMAX        {ROWMAX}
  | COLMAX        {COLMAX}
;

binary:
    /* Binary Operators */
    ADD           {ADD}
  | SUBT          {SUBT}
  | MULT          {MULT}
  | DIV           {DIV}
;
