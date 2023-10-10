type case = Vide | Noir | Char of string
    
(* let dict = "./words_dict/french_sans_spec_char" *)
(*let dict = "./words_dict/french_test"
let words_dict = Dict.words dict*)



let ouvre_en_lecture file =
  try
    open_in file
  with
    exc ->
      Printf.printf "%s ne s'ouvre pas en lecture\n" file;
      raise exc

let input_option = fun ch ->
  try Some (input_line ch) with End_of_file -> close_in ch ; None
        


let char2case char =
  match char with
    "." -> Vide
  | "#" -> Noir
  | h -> Char h

        
let print_case case =
  match case with
    Vide -> Printf.printf "%d\t" 0
  | Noir -> Printf.printf "%d\t" 1
  | Char s -> Printf.printf "%s\t" s 
        
  
let read file =
  let ch_file = ouvre_en_lecture file in
  let line = input_line ch_file in
  let m = String.length line in
  let rec read_rec lines =
    match input_option ch_file with
      None -> List.rev lines
    | Some line ->
        if String.length line <> m then failwith "Test.read : lenght differs";
        read_rec (line::lines)
  in
  let lines = Array.of_list (line::read_rec []) in

  (*Array.map (string -> string array) lines;*)

  let sep = Str.regexp " +" in
  let lines = Array.map (fun line -> Array.of_list (Str.split sep line)) lines in
    
  Array.map (Array.map char2case) lines



 

  (*

let () =
        
  let grid_file = "./grid.txt" in

  
  let dict = "./words_dict/french_sans_spec_char" in 
  (*let dict = "./words_dict/french_test" in*)
  let words_dict = Dict.words dict in
  

    (*
    let case_vide = "." in
    let case_noire = "#" in

   *)

  

  let grid = read grid_file in
  (*Dict.print_list l*)
  (*Dict.print_array (Printf.printf "%s\n") l*)
  Dict.print_matrix print_case grid;


  let list_mots, cpt_mots, cpt_lignes, contraintes_lettres, debut, indice_j, length, sens  = contraintes2 grid words_dict in
  Printf.printf "cpt_mots : %d\tcpt_lignes : %d\n" cpt_mots cpt_lignes;
  intersect_total list_mots;
  print_list_mot list_mots;

  
*)
