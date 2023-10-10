(* Fonction qui ouvre un fichier en lecture *)
let ouvre_en_lecture file =
  try
    open_in file
  with
    exc ->
      Printf.printf "%s ne s'ouvre pas en lecture\n" file;
      raise exc

let input_option = fun ch ->
  try
    Some (input_line ch)
  with
    End_of_file ->
      close_in ch ;
      None

        
(* Fonction qui renvoie la longueur du mot maximal dans le dictionnaire et la liste de tous les mots du dictionnaire*)
let list_mots file =
  let ch_file = ouvre_en_lecture file in
  let rec aux max list_mots =
    match input_option ch_file with
      None -> max, list_mots
    | Some line ->
        let length = String.length line in
        aux (if length>max then length else max) (line::list_mots)
  in
  aux 0 []


(*Creer le tableau des mots indexé par le nombre de lettres*)
let words file =
  let max, list_mots = list_mots file in
  let words = Array.make (max+1) [] in
  let rec aux words list_mots =
    match list_mots with
      [] -> words
    | mot::t ->
        let length = String.length mot in
        words.(length) <- mot::words.(length);
        aux words t 
  in
  aux words list_mots



(***************************************************************************)    
      
(* Fonction qui affiche un vecteur *)      
let print_array print array =
  let n = Array.length array in
  for i=0 to (n-1) do
    print array.(i)
  done;
  print_endline "" (* Saute une ligne *)
    
    
(* Fonction qui afiche une liste *)
let rec print_list list =
  match list with
    [] -> print_endline ""
  | h::t -> Printf.printf "%s\t" h ; print_list t
