
let main () =
	  let with_gc = ref false in
		let print_mem_size = ref false in 
    let src = ref "" in
    let spec = [("-gc", Arg.Set with_gc, "Execute the input program with a garbage collector");
								("-print_mem_size", Arg.Set print_mem_size, "Execute the input program with a garbage collector")
							 ] 
		in
    let usage = "Usage: run <options> <file>" in
    let _ = Arg.parse spec
                (fun
                   x ->
                     if Sys.file_exists x then src := x
                     else raise (Arg.Bad (x ^ ": No files given")))
                usage
    in

  if !src = "" then Arg.usage spec usage
  else
    let file_channel = open_in !src in
    let lexbuf = Lexing.from_channel file_channel in
    let pgm = Parser.program Lexer.start lexbuf in
	  try
			C.run pgm !with_gc !print_mem_size
  	with Lexer.LexicalError -> print_endline (!src ^ ": Lexical Error")
		
let _ = main ()
