%{

%}

%token <int> NUM
%token <string> ID
%token INT PLUS MINUS STAR SLASH EQUAL EQUALEQUAL LE LT GE GT NOT AND OR IF ELSE
    WHILE DO READ PRINT SEMICOLON BEGIN END VAR LET IN PROC SKIP
%token LBRACE RBRACE LBLOCK RBLOCK LPAREN RPAREN EOF TRUE FALSE 
%token COMMA DOT

%left COMMA DOT
%left LT EQUALEQUAL
%left PLUS MINUS
%left STAR SLASH 
%right NOT 
%nonassoc RPAREN
%nonassoc ELSE
%left SEMICOLON


%start program
%type <C.program> program
%%

program:
    exp EOF { $1 }
    ;

exp:
  | SKIP { C.SKIP }
	| TRUE { C.TRUE }
  | FALSE { C.FALSE } 
	| NUM { C.CONST $1 }
  | ID { C.VAR $1 } 
	| LPAREN exp RPAREN  { $2 }
	| exp PLUS exp  { C.ADD ($1,$3) }
  | exp MINUS exp { C.SUB ($1,$3) }
  | exp STAR exp  { C.MUL ($1,$3) }
	| exp SLASH exp  { C.DIV ($1,$3) } 
  | exp LE exp { C.LE ($1,$3) }  
	| exp EQUALEQUAL exp { C.EQ ($1,$3) }
	| NOT exp { C.NOT $2 }
	| IF LPAREN exp RPAREN LBRACE exp RBRACE ELSE LBRACE exp RBRACE { C.IF ($3,$6,$10) }
	| WHILE LPAREN exp RPAREN LBRACE exp RBRACE {C.WHILE ($3,$6)}     
  | LET ID EQUAL exp IN exp { C.LET ($2,$4,$6) }
	| PROC LPAREN vargs RPAREN exp { C.PROC ($3,$5) }
	| exp LPAREN args RPAREN { C.CALLV ($1,$3) }
	| exp LBLOCK vargs RBLOCK { C.CALLR ($1,$3) } 
	| ID EQUAL exp { C.ASSIGN ($1, $3) }
	| LBRACE records RBRACE { C.RECORD ($2) } 
	| exp DOT ID { C.FIELD ($1, $3) }  
  | exp DOT ID EQUAL exp { C.ASSIGNF ($1, $3, $5) }
	| exp SEMICOLON exp { C.SEQ ($1, $3) }
  | exp SEMICOLON { $1 }
	| BEGIN exp END { C.BEGIN $2 }  
	| READ LPAREN ID RPAREN { C.READ $3 }
  | PRINT LPAREN exp RPAREN { C.PRINT $3 }
  ;

args: { [] }
  | exp { [$1] }
  | args COMMA exp { $1 @ [$3] }
  ;

vargs: { [] }
  | ID { [$1] }
  | vargs COMMA ID { $1 @ [$3] }
  ;

records: { [] } 
	| ID EQUAL exp { [ ($1, $3) ] }
	| records COMMA ID EQUAL exp { $1 @ [($3, $5)] } 
	;
%%

let parse_error s = print_endline s
