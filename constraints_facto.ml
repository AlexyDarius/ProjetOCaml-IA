(* type case = Vide | Noir | Char of string *) 



(* Fonction qui permet de matcher l'ensemble des mots qui vérifient une contrainte (regexp)*)
let match_string list_words regexp =
  let rec aux list_words =
    match list_words with
      [] -> []
    | h::t ->
        if Str.string_match regexp h 0
        then h::(aux t) (* si h vérifie la regexp alors on le garde dans la liste de mots *)
        else aux t (* sinon on passe à l'élément suivant *)
  in
  aux list_words
    
    
    
    
(* Fonction qui prend une liste de mots et de contraintes et qui renvoie la liste des mots qui vérifient les contraintes *)        
let constraints list_words contraintes =
  let rec aux list_words contraintes = 
    
    match contraintes with
      [] -> list_words
    | (indice, lettre, debut, fin)::t ->
        
        let s = String.make (indice-debut) '.' in
        let s = s ^ lettre in
        let s1 = String.make (fin-indice) '.' in
        let s = s ^ s1 in
        let regexp = Str.regexp s in
        let result = match_string list_words regexp in
        aux result t
  in
  aux list_words contraintes





    
type contraintes = {indice : int ; lettre : string ; debut : int ; fin : int} 
    
type mot = {mutable words : string list ; mutable previous_words : string list ; constraints : (int*string*int*int) list ; id : int ; sens : string ; debut : int*int ;
            fin : int*int ; length : int ; mutable intersect : (int * (int*int)) list}


(* Fonctions qui permettent de print les différents types crées *)
let rec print_list list l_ou_c =
  match list with
    [] -> print_string "liste vide\n"
  | (lc, debut, fin, length)::[] -> Printf.printf "%s : %d, debut : %d, fin : %d, length : %d\n" l_ou_c lc debut fin length
  | (lc, debut, fin, length)::t -> Printf.printf "%s : %d, debut : %d, fin : %d, length : %d\n" l_ou_c lc debut fin length ; print_list t l_ou_c

let rec print_string_list words =
  match words with
    [] -> print_endline ""
  | h::t -> Printf.printf "%s\t" h ; print_string_list t
        
let rec print_constraints_list constraints =
  match constraints with
    [] -> print_endline ""
  | (indice, lettre,  debut, fin)::t -> Printf.printf "Indice : %d, Lettre : %s, Debut : %d, Fin : %d\t" indice lettre debut fin ; print_constraints_list t

let rec print_list_intersect2 list_intersect =
  match list_intersect with
    [] -> print_endline ""
  | (b,(c,d))::t -> Printf.printf "Id_mot_inter : %d, Intersection : (%d, %d)\n" b c d ; print_list_intersect2 t

        
let rec print_list_mot list_mot =
  match list_mot with
    [] -> print_endline ""
  | h::t -> print_string "Words :\n" ; print_string_list h.words ;  print_string "Constraints : "; print_constraints_list h.constraints ; Printf.printf "Id : %d\n%!" h.id ;
      Printf.printf "Sens : %s\n%!" h.sens;
      let i,j = h.debut in Printf.printf "Debut : (%d, %d)\t%!" i j ; let i,j = h.fin in Printf.printf "Fin : (%d, %d)\t" i j ; Printf.printf "length
      t : %d\n" h.length ;
      print_string "Intersection : " ; print_list_intersect2 h.intersect ; print_string "\n"; print_list_mot t

let rec print_list_intersect list_intersect =
  match list_intersect with
    [] -> print_endline ""
  | (a,b,(c,d))::t -> Printf.printf "Id_mot1 : %d , Id_mot2 : %d, Intersection : (%d, %d)\n" a b c d ; print_list_intersect t

let rec print_list_intersect2 list_intersect =
  match list_intersect with
    [] -> print_endline ""
  | (b,(c,d))::t -> Printf.printf "Id_mot2 : %d, Intersection : (%d, %d)\n" b c d ; print_list_intersect2 t

let rec print_intersect_list_mots list_mots =
  match list_mots with
    [] -> print_endline ""
  | mot::t -> Printf.printf "Id_mot1 : %d\n" mot.id ; print_list_intersect2 mot.intersect ; print_intersect_list_mots t

(************************************************************)



let transpose matrix =
  let n, m = Array.length matrix, Array.length matrix.(0) in
  let tr = Array.make_matrix m n Readfile.Vide in
    
  (*
  for i=0 to n-1 do
      for j=0 to m-1 do
        tr.(j).(i) <- matrix.(i).(j)
      done
  done;
  tr
   *)

  let f (tr, cpt_ligne, cpt_column) case =
    if cpt_column = m-1
    then
      begin
        tr.(cpt_column).(cpt_ligne) <- case ; tr, cpt_ligne+1, 0
      end
    else
      begin
        tr.(cpt_column).(cpt_ligne) <- case ; tr, cpt_ligne, cpt_column + 1
      end
  in
  Array.fold_left (Array.fold_left f) (tr, 0, 0) matrix


    
let ordonne_mots = fun liste_mots -> 
  let compare_intersect = fun l1 l2 -> 
    let ordre = -Stdlib.compare (List.length l1.intersect) (List.length l2.intersect) in
    if ordre = 0 
    then (-Stdlib.compare (List.length l1.constraints) (List.length l2.constraints)) 
    else ordre
  in 
  List.sort (compare_intersect) liste_mots

let contraintes grid words_dict =
  
  
  let n, m = Array.length grid, Array.length grid.(0) in
  (* Printf.printf "n : %d\tm : %d\n" n m;*)

   (* grid de type  case array array
          f : list*int*int -> case array -> list*int*int  *)

  let ajoute y x =
    let a,b,c = x in
    (a,b,c,y)
  in
  (* Fonctio*)
  let constructeur_mots (list_mots, cpt_mots, cpt_lignes, contraintes_lettres, debut, indice_j, length, sens) case =
    let max =
      if sens = "horizontal" then m-1
      else if sens = "vertical" then n-1
      else failwith "No word direction" in
    let coords_debut =
      if sens = "horizontal" then (cpt_lignes, debut)
      else if sens = "vertical" then (debut, cpt_lignes)
      else failwith "No word direction" in
    let coords_fin =
      if sens = "horizontal" then (cpt_lignes, indice_j)
      else if sens = "vertical" then (indice_j, cpt_lignes)
      else failwith "No word direction" in
    
    let stop_avant_noir x =
      let a,b = x in
      if sens = "horizontal" then (a, b-1)
      else if sens = "vertical" then (a-1, b)
      else failwith "No word direction"
    in

  let traitement_contraintes l p1 p2 p3 p4 p5 p6 p7 p8 ending =
    if length > l
      then
        {words = constraints words_dict.(length+p1) (List.map (ajoute (indice_j+p2)) contraintes_lettres) ;
        constraints = List.map (ajoute (indice_j+p2)) contraintes_lettres ;
        id = cpt_mots+1 ; sens = sens ;
        debut = coords_debut ; fin = (ending) ;
        length = length+p3 ; intersect = []; previous_words=[]}::list_mots , cpt_mots+1, cpt_lignes+p4,
        [], 0, 0, 0, sens
      else
        list_mots, p5, p6, [], p7, p8, 0, sens
      in
    
    (* let list_mots = ordonne_mots list_mots in *)
    match case with
      Readfile.Vide ->
        if indice_j = max
        then
          traitement_contraintes 0 1 0 1 1 cpt_mots cpt_lignes+1 0 0 coords_fin
          (*if length > 0
          then
            {words = constraints words_dict.(length+1) (List.map (ajoute (indice_j)) contraintes_lettres) ;
             constraints = List.map (ajoute indice_j) contraintes_lettres ;
             id = cpt_mots+1 ; sens = sens ;
             debut = coords_debut ; fin = coords_fin ;
             length = length+1 ; intersect = []; previous_words=[]}::list_mots , cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens*)
        else
          list_mots, cpt_mots, cpt_lignes, contraintes_lettres, debut, indice_j+1, length+1, sens
    | Noir ->
        if indice_j = max
        then
          if length > 1
          then
            traitement_contraintes 1 0 -1 0 1 cpt_mots cpt_lignes+1 0 0
            (*{words = constraints words_dict.(length) (List.map (ajoute (indice_j-1)) contraintes_lettres) ;
             constraints = List.map (ajoute (indice_j-1)) contraintes_lettres ;
             id = cpt_mots+1 ; sens = sens ;
             debut = coords_debut ; fin = (stop_avant_noir coords_fin) ;
             length = length ; intersect = []; previous_words=[]}::list_mots , cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens*)
        else
          traitement_contraintes 1 0 -1 0 0 cpt_mots cpt_lignes indice_j+1 indice_j+1 (stop_avant_noir coords_fin)
          (*if length > 1
          then
            {words = constraints words_dict.(length) (List.map (ajoute (indice_j-1)) contraintes_lettres) ;
              constraints = List.map (ajoute (indice_j-1)) contraintes_lettres ;
              id = cpt_mots+1 ; sens = sens ;
              debut = coords_debut ; fin = (stop_avant_noir coords_fin) ;
              length = length ; intersect = []; previous_words=[]}::list_mots , cpt_mots+1, cpt_lignes,
            [], indice_j+1, indice_j+1, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes, [], indice_j+1, indice_j+1, 0, sens*)
            
    | Char s ->
        let contraintes_lettres = (indice_j, s, debut)::contraintes_lettres in
        if indice_j = max
        then
          traitement_contraintes 0 1 0 1 1 cpt_mots cpt_lignes 0 0 coords_fin
          (*if length > 0
          then
            {words = constraints words_dict.(length+1) (List.map (ajoute indice_j) contraintes_lettres) ;
             constraints = List.map (ajoute indice_j) contraintes_lettres ;
             id = cpt_mots+1 ; sens = sens ;
             debut = coords_debut ; fin = coords_fin ;
             length = length+1 ; intersect = []; previous_words=[]}::list_mots , cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens*)
        else
          list_mots, cpt_mots, cpt_lignes, contraintes_lettres, debut, indice_j+1, length+1, sens       
  in
  
  let list_mots, cpt_mots, _, _, _, _, _, _ =
    Array.fold_left (Array.fold_left constructeur_mots) ([], 0, 0, [], 0, 0, 0, "horizontal") grid in
  let grid_transpose, _, _ = transpose grid in
  Array.fold_left (Array.fold_left constructeur_mots) (list_mots, cpt_mots, 0, [], 0, 0, 0, "vertical") grid_transpose
          


    
let test_intersect mot_horizontal list_mots i j1 j2 =
  let rec aux list_mots =
    match list_mots with
      [] -> ()
    | mot::t ->
        if mot.sens = "vertical"
        then
          let i1, j = mot.debut in
          let i2, _ = mot.fin in
          if i1<=i && i<=i2 && j1<=j && j<=j2
          then
            begin
              mot_horizontal.intersect <- (mot.id, (i,j))::mot_horizontal.intersect ;
              mot.intersect <- (mot_horizontal.id, (i,j))::mot.intersect ;
              aux t
            end
          else aux t
        else aux t
  in
  aux list_mots
    
let intersect_total list_mots =
  let rec aux mots =
    match mots with
      [] -> ()
    | mot::t ->
        if mot.sens = "horizontal"
        then
          begin
            let i, j1 = mot.debut in
            let _, j2 = mot.fin in
            test_intersect mot list_mots i j1 j2;
            aux t;
          end
        else aux t
  in
  aux list_mots

let rec liste_sans_word list_words word =
  (*match mot_words with
    [] -> failwith "erreur"
  | word::t ->*)
  match list_words with
    [] -> []
  | h::t ->
      if h = word then liste_sans_word t word
      else h::(liste_sans_word t word)
                
let rec liste_sans_mot_words list_words mot_words =
  match mot_words with
    [] -> failwith "erreur"
  | word::[] -> liste_sans_word list_words word
  | word::t -> liste_sans_mot_words (liste_sans_word list_words word) t
        
        
let rec fusion list_words list_mots_assigne =
  match list_mots_assigne with
    [] -> list_words
  | mot::t ->
     (* match mot.words with
        (* mot.words : list words *)
        [] -> failwith "erreur"
      | word::t ->
        aux list_words word*)
      fusion (liste_sans_mot_words list_words mot.words) t






        

    (*
let () =
        

  let grid_file = "./grid.txt" in

  
  let dict = "./words_dict/french_sans_spec_char" in 
  (* let dict = "./words_dict/french_test" in *)
  let words_dict = Dict.words dict in
  


  

  let grid = Readfile2.read grid_file in
  (*Dict.print_list l*)
  (*Dict.print_array (Printf.printf "%s\n") l*)
  Dict.print_matrix Readfile2.print_case grid;


  let list_mots, cpt_mots, cpt_lignes, contraintes_lettres, debut, indice_j, length, sens = contraintes grid words_dict in
  Printf.printf "cpt_mots : %d\tcpt_lignes : %d\n" cpt_mots cpt_lignes;
  intersect_total list_mots;
  (* print_list_mot list_mots; *)



  print_list_mot (ordonne_mots list_mots);
 *)
