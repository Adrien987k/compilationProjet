let _ =

  (* Ouverture un flot de caractère ; ici à partir du fichier cmds.txt' stdin  *)  
  let source = Lexing.from_channel (open_in "cmds.txt") in 

  (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
  let rec f () =
    try
      (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
	let q = Parser.ansyn Lexer.anlex source in
	     Printf.printf "%s\n" (Ast.string_of_query q); flush stdout;
      f ()
    with Lexer.Eof -> Printf.printf "Bye\n"
  in

  f ()

