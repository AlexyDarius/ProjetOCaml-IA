let () =
  
  Printf.printf "Saisissez le chemin d'accès au fichier :\n";
  let grid_file = read_line() in 
  (*let grid_file = "grid4" in*)
  let grid = Readfile.read grid_file in


  (*Printf.printf "Saisissez le chemin d'accès au dictionnaire :\n";
  let dict = read_line() in *)
  let dict = "./words_dict/dico.txt" in

  let words_dict = Dict.words dict in

  (* Printf.printf "Longeur mot 5 lettres : %d\n" (List.length words_dict.(5)); *)
  
  Interface.print_matrix grid;
  
  let list_mots = Constraints.create_list_mots grid words_dict in
  Constraints.print_list_mot list_mots;

  let ordered_list_mots = Backtrack.ordonne_mots_dynamic list_mots in 
  
  let debut = Sys.time() in 
  let result = Backtrack.backtrack ordered_list_mots in
  match result with
    None ->
      print_string "PAS DE SOLUTION\n" ;
      Printf.printf "Temps d'execution: %f\n" (Sys.time() -. debut)
  | Some s ->
      print_string "BRAVO, VOUS AVEZ TROUVE UNE SOLUTION : \n" ;
      Printf.printf "Temps d'execution: %f\n" (Sys.time() -. debut);
      Interface.print_matrix (Interface.put_liste grid s)


