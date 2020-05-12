{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [
                    ("int", INT);
                    ("if", IF);
                    ("else", ELSE);
                    ("while", WHILE);
                    ("print", PRINT);
                    ("read", READ);
                    ("true", TRUE);
                    ("false", FALSE);
                    ("begin", BEGIN);
                    ("end", END);
										("let", LET);
										("in", IN);
										("proc", PROC);
										("skip", SKIP);
                  ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
 parse blank { start lexbuf }
     | "/*" { comment_depth :=1; comment lexbuf; start lexbuf }
     | number { NUM (int_of_string (Lexing.lexeme lexbuf)) }
     | id { let id = Lexing.lexeme lexbuf
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | ","   { COMMA }
		 | "."   { DOT } 
		 | "+"   { PLUS }
     | "-"   { MINUS }
     | "*"   { STAR }
		 | "/"   { SLASH } 
     | "!"   { NOT }
     | "="   { EQUALEQUAL }
     | ":="  { EQUAL }
     | "<="  { LE }
     | ";"   { SEMICOLON }
     | "("   { LPAREN }
     | ")"   { RPAREN }
		 | "<"   { LBLOCK }
     | ">"   { RBLOCK }		
     | "{"   { LBRACE }
     | "}"   { RBRACE } 
     | eof   { EOF}
     | _ { raise LexicalError }

and comment = parse
     "/*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*/" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
