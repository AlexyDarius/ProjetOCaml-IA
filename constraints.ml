type sens = Horizontal | Vertical
    
type contraintes = {indice : int ; lettre : string ; debut : int ; fin : int} 

type intersect = {id_inter : int ; coords : int * int}
      
type mot = {
    id : int;
    coords_debut : int * int;
    coords_fin : int * int;
    length : int;
    sens : sens;
    mutable domain : string list ;
    constraints : contraintes list ;
    intersect : intersect list
  }



(* Fonction qui permet de matcher l'ensemble des mots qui 
vérifient une contrainte (regexp)*)
let match_string list_words regexp =
  let rec aux list_words =
    match list_words with
      [] -> []
    | h::t ->
        if Str.string_match regexp h 0
        then h::(aux t)                  (* si h vérifie la regexp alors on le garde dans la liste de mots *)
        else aux t                       (* sinon on passe à l'élément suivant *)
  in
  aux list_words
    
    
(* Fonction qui prend une liste de mots et de contraintes et qui
 renvoie la liste des mots qui vérifient les contraintes *)        
let rec words_w_constraints list_words list_contraintes =
  match list_contraintes with
    [] -> list_words
  | contrainte::t ->
      let indice = contrainte.indice in
      let lettre = contrainte.lettre in
      let debut = contrainte.debut in
      let fin = contrainte.fin in

      
      let s = String.make (indice-debut) '.' in
      let s = s ^ lettre in
      let s1 = String.make (fin-indice) '.' in
      let s = s ^ s1 in
      let regexp = Str.regexp s in
      let result = match_string list_words regexp in
      words_w_constraints result t

        


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




    
(* Fonctions permettant d'ajouter la liste des intersections pour chaque mot *)
(* ajoute pour un mot donne sa liste des intersections *)
let test_intersect mot_horizontal list_mots i j1 j2 new_list_mots =
  let rec aux list_mots new_list_mots =
    match list_mots with
      [] -> new_list_mots
    | mot::t ->
        if mot.sens = Vertical
        then
          let i1, j = mot.coords_debut in
          let i2, _ = mot.coords_fin in
          if i1<=i && i<=i2 && j1<=j && j<=j2    (* test d'intersection entre 2 mots *)
          then
            begin
              let add_intersect elt =
                if elt.id = mot_horizontal.id
                then {elt with intersect = {id_inter = mot.id ; coords = (i,j)}::elt.intersect}
                else if elt.id  = mot.id
                then {elt with intersect = {id_inter = mot_horizontal.id ; coords = (i,j)}::elt.intersect}
                else elt
              in
                    
              let new_list_mots = List.map add_intersect new_list_mots in 
              aux t new_list_mots
            end
          else aux t new_list_mots
        else aux t new_list_mots
  in
  aux list_mots new_list_mots

    
(* renvoie la liste des mots en ajoutant la liste des
 intersections pour chaque mot *)
let intersect list_mots =
  let rec aux mots new_list_mots =
    match mots with
      [] -> new_list_mots
    | mot::t ->
        if mot.sens = Horizontal   (* on ne cherche les intersections que sur les mots horizontaux ce qui nous les donne toutes *)
        then
          begin
            let i, j1 = mot.coords_debut in
            let _, j2 = mot.coords_fin in
            let new_list_mots = test_intersect mot list_mots i j1 j2 new_list_mots in
            aux t new_list_mots
          end
        else aux t new_list_mots
  in
  aux list_mots list_mots


    


(* creer la liste des mots de la grille avec leurs attributs
 (id, coords_debut, coords_fin, length, sens, domaine, constraints, intersect)
 de la grille *)
let create_list_mots grid words_dict =
  
  let n, m = Array.length grid, Array.length grid.(0) in
 
  let ajoute_mots list_mots length fin list_contraintes id sens coords_debut coords_fin =
    let list_contraintes = List.map (fun elt -> {elt with fin = fin}) list_contraintes in
    {id = id;
     coords_debut = coords_debut;
     coords_fin = coords_fin;
     length = length;
     sens = sens;
     domain = words_w_constraints words_dict.(length) list_contraintes ;
     constraints = list_contraintes ;
     intersect = []}::list_mots
  in
  
  (* creer la liste des mots de la grille grâce à fold_left *)
  let constructeur_mots (list_mots, cpt_mots, cpt_lignes, list_contraintes, debut, indice_j, length, sens) case =
    let max =
      match sens with
        Horizontal -> m-1
      | Vertical -> n-1
    in

    let coords_debut =
      match sens with
        Horizontal -> (cpt_lignes, debut)
      | Vertical -> (debut, cpt_lignes)
    in
   
    let coords_fin =
      match sens with
        Horizontal -> (cpt_lignes, indice_j)
      | Vertical -> (indice_j, cpt_lignes)
    in
    
    let stop_avant_noir x =
      let a,b = x in
      match sens with
        Horizontal -> (a, b-1)
      | Vertical -> (a-1, b)
    in
    
    match case with
      Readfile.Vide ->
        if indice_j = max
        then
          if length > 0
          then
            ajoute_mots list_mots (length+1) (indice_j) list_contraintes (cpt_mots+1) sens coords_debut coords_fin, cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens
        else
          list_mots, cpt_mots, cpt_lignes, list_contraintes, debut, indice_j+1, length+1, sens   
  
    | Readfile.Noir ->
        if indice_j = max
        then
          if length > 1
          then
            ajoute_mots list_mots length (indice_j-1) list_contraintes (cpt_mots+1) sens coords_debut (stop_avant_noir coords_fin), cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens
        else
          if length > 1
          then
            ajoute_mots list_mots length (indice_j-1) list_contraintes (cpt_mots+1) sens coords_debut (stop_avant_noir coords_fin), cpt_mots+1, cpt_lignes,
            [], indice_j+1, indice_j+1, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes, [], indice_j+1, indice_j+1, 0, sens
            
    | Readfile.Char s ->
        let list_contraintes = {indice = indice_j ; lettre = s ; debut = debut ; fin = 0}::list_contraintes in
        if indice_j = max
        then
          if length > 0
          then
            ajoute_mots list_mots (length+1) indice_j list_contraintes (cpt_mots+1) sens coords_debut coords_fin, cpt_mots+1, cpt_lignes+1,
            [], 0, 0, 0, sens
          else
            list_mots, cpt_mots, cpt_lignes+1, [], 0, 0, 0, sens
        else
          list_mots, cpt_mots, cpt_lignes, list_contraintes, debut, indice_j+1, length+1, sens       
  in

  try 
    let list_mots, cpt_mots, _, _, _, _, _, _ =
      Array.fold_left (Array.fold_left constructeur_mots) ([], 0, 0, [], 0, 0, 0, Horizontal) grid in
    let grid_transpose, _, _ = transpose grid in
    let list_mots, _, _, _, _ ,_ ,_ ,_ = Array.fold_left (Array.fold_left constructeur_mots) (list_mots, cpt_mots, 0, [], 0, 0, 0, Vertical) grid_transpose in
    (* ajoute les intersections associées à chaque mot *)
    intersect list_mots
  with
    exc -> Printf.printf "Dict pas assez fourni\n" ; raise exc
        
        
   

  
(* Fonctions qui permettent d'enlever du domaine des mots non assigne le mot
 déjà mis dans la grille.
Permet de ne pas avoir 2 fois le même mot dans la grille *)
(* enlève de list_words le mot word (de type str) *)
let rec liste_sans_word list_words (word: string) =
  match list_words with
    [] -> []
  | h::t ->
      if h = word then liste_sans_word t word
      else h::(liste_sans_word t word)

(* enlève tous les mots de mot_words présent dans list_words *)
let rec liste_sans_mot_words list_words (mot_words: string list) =
  match mot_words with
    [] -> failwith "erreur"
  | word::[] -> liste_sans_word list_words word
  | word::t -> liste_sans_mot_words (liste_sans_word list_words word) t
        
(* Enlève de list_words les mots présent dans list_mots_assigne *)      
let rec delete_duplicate list_words list_mots_assigne =
  match list_mots_assigne with
    [] -> list_words
  | mot::t ->
      delete_duplicate (liste_sans_mot_words list_words mot.domain) t




(* Fonctions qui permettent de print les différents types crées *)

let print_sens mot =
  match mot.sens with
    Horizontal -> Printf.printf "Sens : horizontal\n"
  | Vertical -> Printf.printf "Sens : vertical\n"
        
let rec print_list_string words =
  match words with
    [] -> print_endline ""
  | h::t -> Printf.printf "%s\t" h ; print_list_string t
        
let rec print_list_constraints constraints =
  match constraints with
    [] -> print_endline ""
  | contraintes::t ->
      Printf.printf "Indice : %d, Lettre : %s, Debut : %d, Fin : %d\t" contraintes.indice contraintes.lettre contraintes.debut contraintes.fin ; print_list_constraints t

        
let tuple_int_printer ppf (x,y) =
  Printf.fprintf ppf "(%d, %d)" x y
    
let rec print_list_intersect list_intersect =
  match list_intersect with
    [] -> print_endline ""
  | intersect::t -> Printf.printf "Id_mot_inter : %d, Intersection : %a\n" intersect.id_inter tuple_int_printer intersect.coords ; print_list_intersect t

        
let rec print_list_mot list_mot =
  match list_mot with
    [] -> print_endline ""
  | h::t ->
      Printf.printf "Id : %d\n%!" h.id ;
      let i,j = h.coords_debut in Printf.printf "Debut : (%d, %d)\t%!" i j ;
      let i,j = h.coords_fin in Printf.printf "Fin : (%d, %d)\t" i j ;
      Printf.printf "lengtht : %d\n" h.length ;
      print_sens h ;
      Printf.printf "Words : " ;
      print_list_string h.domain ;
      Printf.printf "Constraints : ";
      print_list_constraints h.constraints ;
      Printf.printf "Intersection : " ;
      print_list_intersect h.intersect ;
      Printf.printf "\n";
      print_list_mot t
