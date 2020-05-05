{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [
                    ("iszero", ISZERO);
                    ("if", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("let",LET);
                    ("in",IN);
                    ("letrec",LETREC);
                    ("begin",BEGIN);
                    ("end",END);
                    ("setref",SETREF);
                    ("deref",DEREF);
                    ("newref",NEWREF);
                    ("read",READ);
                    ("proc",PROC);
                  ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']+

rule start =
 parse blank { start lexbuf }
     | "/*" { comment_depth :=1; comment lexbuf; start lexbuf }
     | digit { NUM (int_of_string (Lexing.lexeme lexbuf)) }
     | id { let id = Lexing.lexeme lexbuf
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | ","   { COMMA }
     | ";"   { SEMICOLON }
     | "+"   { PLUS }
     | "-"   { MINUS }
     | "*"   { STAR }
     | "/"   { SLASH }
     | "="   { EQUAL }
     | "<="  { LE }
     | ">="  { GE }
     | "<"   { LT }
     | ">"   { GT }
     | "("   { LPAREN }
     | ")"   { RPAREN }
     | eof   { EOF}
     | _ { raise LexicalError }

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
