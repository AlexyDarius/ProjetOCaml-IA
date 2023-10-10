let backtrack input = 
  let grid_file = input in 
  let grid = Readfile.read grid_file in
  
  let dict = "./words_dict/dico.txt" in
  
  let words_dict = Dict.words dict in

  Interface.print_matrix grid;
  
  let list_mots = Constraints.create_list_mots grid words_dict in

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



let () =
   
let oc = open_out "./benchmark/bench.txt" in
Printf.fprintf oc "Benchmark file\n\n";

let file = "./benchmark/gridlist.txt" in

let ic = open_in file in
     let rec build_list infile =
          try
               let line = input_line infile in
               line :: build_list(infile)
          with End_of_file ->
               close_in infile;
               [] in
        let rec bt = function
          [] -> ()
          | e::l -> 
            let debut = Sys.time() in
            backtrack e; Printf.fprintf oc "%s ; %f\n" e (Sys.time() -. debut); print_string "\n\t end \n\n" ; bt l in bt(build_list(ic));

close_out oc;