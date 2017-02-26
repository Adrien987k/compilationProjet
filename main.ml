

let _ =

  (* Ouverture un flot de caractère ; ici à partir du fichier cmds.txt' stdin  *)  
  let source = Lexing.from_channel (open_in "cmds.txt") in 

  let vin_att, vin = Ast.R.from_file "vin.csv" '|' in
  let viticulteur_att, viticulteur = Ast.R.from_file "viticulteur.csv" '|' in
  let client_att, client = Ast.R.from_file "client.csv" '|' in
  let commande_att, commande = Ast.R.from_file "commande.csv" '|' in

  let env = Env.empty in
  let env = Env.add "vin" (vin, vin_att) env in
  let env = Env.add "viticulteur" (viticulteur, viticulteur_att) env in
  let env = Env.add "client" (client, client_att) env in
  let env = Env.add "commande" (commande, commande_att) env in

  (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
  let rec f () =
    try
      (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
	     let query = Parser.ansyn Lexer.anlex source in

	     Printf.printf "%s\n" (Ast.string_of_query query); flush stdout;
       let res_query = Ast.eval_query env query in
       match res_query with
       | (r, att_env) -> let rec inverse_string_attribute att_env = 
                            (match att_env with 
                            | [] -> []
                            | h :: q -> (match h with
                                        | (s, att) -> (att, s) :: (inverse_string_attribute q)))
                         in
                         let att_env = inverse_string_attribute att_env in
                         Ast.R.print '|' att_env r;
      f ()
    with Lexer.Eof -> Printf.printf "Bye\n"
  in

  f ()

