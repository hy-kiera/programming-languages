%{

%}

%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token INT PLUS MINUS STAR SLASH EQUAL EQUALEQUAL LE LT GE GT NOT AND OR IF THEN
ELSE WHILE DO READ PRINT SEMICOLON LET IN COMMA ISZERO LETREC PROC NEWREF DEREF
SETREF BEGIN END
%token LBRACE RBRACE LBLOCK RBLOCK LPAREN RPAREN EOF

%left SEMICOLON
%left OR
%left AND
%left LT LE GT GE EQUALEQUAL
%left PLUS MINUS
%left STAR SLASH 
%right NOT 


%start program
%type <M.program> program
%%

program:
    exp EOF { $1 }
    ;

exp:
    NUM { M.CONST $1 }
  | ID { M.VAR $1 }
  | exp PLUS exp  { M.ADD ($1,$3) }
  | exp MINUS exp  { M.SUB ($1,$3) }
  | exp STAR exp  { M.MUL ($1,$3) }
  | exp SLASH exp  { M.DIV ($1,$3) }
  | ISZERO exp { M.ISZERO $2 }
  | IF exp THEN exp ELSE exp { M.IF ($2,$4,$6) }
  | LET ID EQUAL exp IN exp { M.LET ($2,$4,$6) }
  | LETREC ID LPAREN ID RPAREN EQUAL exp IN exp { M.LETREC ($2,$4,$7,$9) }
  | PROC LPAREN ID RPAREN exp { M.PROC ($3,$5) } 
  | LPAREN exp exp RPAREN { M.CALL ($2,$3) }
  | NEWREF LPAREN exp RPAREN { M.NEWREF $3 }
  | DEREF LPAREN exp RPAREN { M.DEREF $3 }
  | SETREF LPAREN exp COMMA exp RPAREN { M.SETREF ($3,$5) }
  | exp SEMICOLON exp  { M.SEQ ($1, $3) }
  | LPAREN exp RPAREN { $2 }
  | READ { M.READ }
  | BEGIN exp END { M.BEGIN $2 }
%%

let parse_error s = print_endline s
