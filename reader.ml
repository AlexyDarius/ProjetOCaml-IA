(* Variables globales *)
let _VERTICAL = 0
let _HORIZONTAL = 1

(*Début du programme *)
type case = Vide | Noir | Car of string 

let file = "grid.txt"
let dico = "dico.txt"
let dico_test = "dico_test.txt"

(* Fonction qui met le fichier grille dans une liste et print cette liste *)
let read_grid = fun file ->
  let input_chan = open_in file in (* on ouvre le channel d'entrée *)
  let rec build_list = fun infile ->
    try
      let line = input_line infile in
      line :: build_list infile
    with End_of_file -> (* si on arrive à la fin du fichier texte alors on ferme le fichier d'entrée *)
      close_in infile;
      []
  in
  let rec print_list = fun e ->
    match e with
      [] -> ()
      | e::l -> Printf.printf "%s\n" e ; print_list l in
  print_list(build_list input_chan)

(* Fonction qui donne la longueur du mot le plus long du dictionnaire *)
(*ça marche, mais pas sur le dictionnaire entier: ça fait un stack_overflow, ça marche jusqu'à la ligne ~95000*)
let get_max_length = fun file -> 
  let input_chan = open_in file in (* on ouvre le channel de lecture du dictionnaire *)
  let rec length = fun infile long ->
    try
      let mot = input_line infile in
      (* Printf.printf "%s\n" mot; *) 
      let new_longueur = String.length mot in 
      if new_longueur > long then (length infile new_longueur) else (length infile long) (* si le nouveau mot est plus long, on garde sa longueur, sinon on garde l'ancienne *)
    with End_of_file ->
      close_in infile;
      long in
    length input_chan 1

(* Convertit le dictionnaire en tableau de tableau, ordonnés selon la taille du mot *)
let dico_to_tableau = fun file -> 
  let input_chan = open_in file in 
  let len = (get_max_length file) + 1 in (* on fixe la longueur du tableau, de longueur maximale le plus long mot du dictionnaire *)
  let t = Array.make len [||] in 
  let rec ajoute = fun infile ->
    try 
      let mot = input_line infile in
      (* Printf.printf "%s\n" mot; *)
      let longueur = (String.length mot) in
      t.(longueur) <- Array.append t.(longueur) [|mot|]; (* On ajoute "mot" au tableau déjà contenu dans t, puis on met le nouveau tableau dans t, à la bonne case *)
      ajoute infile
    with End_of_file ->
      close_in infile;
      t in (* pour renvoyer le tableau *)
    ajoute input_chan

let tab = dico_to_tableau dico_test (* Pour pouvoir mémoriser le tableau, et y accéder en dehors de la fonction, à déplacer à terme *)


(****************************************************)
let ouvre_en_lecture file =
  try
    open_in file
  with
    exc ->
      Printf.printf "%s ne s'ouvre pas en lecture\n" file;
      raise exc


let number_lines_columns file =
  let aux ()  = 
    let lines = ref [] in
    let sep = Str.regexp " " in
    
    let rec rec_lecture lines =
      
      let line = input_line file in
      
      let list_string = Str.split sep line in
      let m = List.length list_string in
      (*Printf.printf "%d\n" m;*)
      lines := m::!lines;
      if List.for_all (fun x -> x=m) !lines
      then ()
      else raise (Failure "Lignes pas toutes de même longueur");
      rec_lecture lines
    in
    try rec_lecture lines
    with End_of_file -> close_in file ; !lines
  in
  let list_lines = aux () in
  let n = List.length list_lines in
  let m = List.hd list_lines in
  n,m
    
    
    
let create_array_grid file =
  let grid = ouvre_en_lecture file in
  let n,m = number_lines_columns grid in
  
  let grid = ouvre_en_lecture file in
  let grid_array = Array.make_matrix n m Vide in
  
  let sep = Str.regexp " " in
  
  let rec rec_lecture cpt_line grid_array =
    let l = input_line grid in
    let list = Str.split sep l in
    let cpt_column = ref 0 in
    let rec aux list =
      match list with
        [] -> rec_lecture (cpt_line+1) grid_array
      | h::t ->
          begin
            if h = "."
            then grid_array.(cpt_line).(!cpt_column) <- Vide
            else (if h = "#" 
              then grid_array.(cpt_line).(!cpt_column) <- Noir
              else grid_array.(cpt_line).(!cpt_column) <- Car h);
            incr cpt_column;
            aux t
          end
    in
    aux list
  in
  try rec_lecture 0 grid_array
  with End_of_file -> grid_array
      
      
      
      
let print_array array =
  let n = Array.length array in
  let m = Array.length array.(0) in
  for i=0 to (n-1) do
    for j=0 to (m-1) do
      (*Printf.printf "%d\t" array.(i).(j)*)
      begin
      match array.(i).(j) with
      Vide -> Printf.printf "%d\t" 0
      |Noir -> Printf.printf "%d\t" 1
      | Car s -> Printf.printf "%s\t" s
      end
    done;
      print_endline ""
  done

(* Fonction qui donne le nombre de mots sur une ligne donnée*)
let nombre_mots_ligne = fun grid ligne ->
  let n = Array.length grid.(ligne) in  
  let rec nombre_mots = fun ind acc precedent->
    if ind < n 
    then  
      let case = grid.(ligne).(ind) in 
        match case with 
          Vide | Car _ -> if precedent = Noir 
                          then 
                            begin nombre_mots (ind+1) (acc+1) Vide 
                            end 
                          else nombre_mots (ind+1) acc Vide 
        | Noir -> nombre_mots (ind+1) acc Noir
    else acc in 
  nombre_mots 0 0 Noir (* On commence par noir dans le cas où la première case serait Vide *)

(* fonctions qui donnent la longueur d'un mot dans la grille, pour ensuite pouvoir filtrer, de manière horizontale ou verticale *)
let get_longueur_mot_horiz = fun tab_grid i debut ->
  let rec get_longueur = fun l j ->
    match tab_grid.(i).(j + debut) with 
      Vide | Car _ -> get_longueur (l+1) (j+1)
    | Noir -> l in
    get_longueur 0 0

let get_longueur_mot_vert = fun tab_grid j debut ->
  let rec get_longueur = fun l i ->
    match tab_grid.(i + debut).(j) with 
      Vide | Car _ -> get_longueur (l+1) (i+1)
    | Noir -> l in
    get_longueur 0 0

(* Fonction qui renvoit les coordonnées de début d'un mot et les coordonnées de sa fin, en fonction du sens horizontal ou vertical*)
let get_coords_mot = fun i j sens tab_grid -> 
  if sens = _HORIZONTAL (* on considère que 1 correspond à de l'horizontal, le reste en vertical *)
  then (let long = (get_longueur_mot_horiz tab_grid i j) in [|(i, j); (i, j+long-1)|]) 
  else (let long = (get_longueur_mot_vert tab_grid j i) in [|(i, j); (i+long-1, j)|])

let toutes_coords_ligne = fun grid ligne -> 
  let nb = nombre_mots_ligne grid ligne in 
  let n = Array.length grid.(ligne) in 
  let rec liste_coords = fun liste ind precedent ->
    if (ind < n) && (List.length liste < nb) (* tant qu'on n'est pas arrivé au bout et que l'on n'a pas le bon nombre de mots dans la liste *)
    then  
      let case = grid.(ligne).(ind) in 
      match case with 
        Vide | Car _ -> if precedent = Noir 
                        then
                          begin let coords = get_coords_mot ligne ind _HORIZONTAL grid in 
                                let _, j_init = coords.(0) in
                                let _, j_fin = coords.(1) in 
                                liste_coords (liste @ [[(ligne, j_init); (ligne, j_fin)]]) (ind+1) Vide 
                          end
                        else liste_coords liste (ind+1) Vide (* on n'ajoute pas de nouveau  mot car on "reste dans le même mot" *) 
      | Noir -> liste_coords liste (ind+1) Noir 
    else liste in 
  liste_coords [] 0 Noir (* on commence par Noir dans le cas où la première case serait Vide *)

(* main *)
let () =
  read_grid file; 
  Printf.printf "%d\n\n" (get_max_length dico_test);
  Array.iter (Printf.printf "%s\n") tab.(8); (* Va afficher tous les mots de 8 lettres contenues dans le dictionnaire "dico_test" *)
  Printf.printf "Je suis le mot en position 1 (donc le deuxième) des mots de 8 lettres: %s\n" tab.(8).(1);
  
  let grid_array = create_array_grid "grid.txt" in
  print_array grid_array;
    
  Printf.printf "longueur du mot en horizontal: %d\n" (get_longueur_mot_horiz grid_array 1 2);
  Printf.printf "longueur du mot en vertical: %d\n" (get_longueur_mot_vert grid_array 1 1);

  let tableau_coords = get_coords_mot 1 1 _VERTICAL grid_array in
  let deb_i, deb_j = tableau_coords.(0) in
  let fin_i, fin_j = tableau_coords.(1) in
  Printf.printf "[|(%d, %d); (%d, %d)|]\n\n" deb_i deb_j fin_i fin_j;
(*
  let grid_array_2 = put_word grid_array "toto" (1,1) _HORIZONTAL in 
  let grid_array_3 = put_word grid_array_2 "etes" (0,1) _VERTICAL in
  print_array grid_array_3;
*)
  Printf.printf "Il y a %d mots sur la ligne choisie\n" (nombre_mots_ligne grid_array 2);
