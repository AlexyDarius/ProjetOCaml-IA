exception Ecriture_sur_case_noire
exception Conflit_de_lettres


let print_case case =
  match case with
    Readfile.Vide -> Printf.printf "%s%!" "." 
  | Noir -> Printf.printf "%s%!" "#"
  | Char s -> Printf.printf "%s%!" (String.uppercase_ascii s) 
        

let print_matrix matrix =
  let m = Array.length matrix.(0) in
  let separation = String.make (m*4+1) '-' in
  Printf.printf " %s\n%!" separation;
  let g col case =
    if col < m-1
    then
      begin
        Printf.printf " | %!";
        print_case case ;
        col+1
      end
    else
      begin
        Printf.printf " | %!";
        print_case case ;
        Printf.printf " |%!";
        Printf.printf "\n %s\n%!" separation;
        0
      end
  in
  
  let _ = Array.fold_left (Array.fold_left g) 0 matrix in
  ()
    


    
(* Fonction qui permet de placer des mots dans la grille,
 de manière horizontale et verticale, prise en compte des conflits sur la grille,
 mais pas les sorties de grille *)
 
let put_word_horiz = fun grid word coords ->
  let i, j = coords in
  let l = String.length word in 
  let rec put_letter_horiz = fun ind n ->
    if n < l
    then 
      let case = grid.(i).(ind) in 
      let lettre = word.[n] in 
        match case with 
          Readfile.Vide -> grid.(i).(ind) <- Char (String.make 1 lettre);
                  put_letter_horiz (ind+1) (n+1)
        | Noir -> raise Ecriture_sur_case_noire
        | Char s ->  if (String.make 1 lettre) = s 
                    then 
                      begin grid.(i).(ind) <- Char (String.make 1 lettre); 
                      put_letter_horiz (ind+1) (n+1) 
                      end
                    else raise Conflit_de_lettres
    else grid in 
  put_letter_horiz j 0
  
let put_word_vert = fun grid word coords ->
  let i, j = coords in
  let l = String.length word in 
  let rec put_letter_vert = fun ind n ->
    if n < l 
    then let case = grid.(ind).(j) in 
    let lettre = word.[n] in 
      match case with 
        Readfile.Vide -> grid.(ind).(j) <- Char (String.make 1 lettre); 
                put_letter_vert (ind+1) (n+1) 
      | Noir -> raise Ecriture_sur_case_noire
      | Char s ->if (String.make 1 lettre) = s 
                then
                  begin grid.(ind).(j) <- Char (String.make 1 lettre);
                  put_letter_vert (ind+1) (n+1)
                  end
                else raise Conflit_de_lettres
    else grid in 
  put_letter_vert i 0

    
(* Fonction qui fusionne les deux fonctions précédentes, plus pratique à utiliser *)
let put_word = fun grid word coords sens -> 
  match sens with
    Constraints.Horizontal -> put_word_horiz grid word coords
  | Constraints.Vertical -> put_word_vert grid word coords


let rec put_liste = fun grid liste_mots -> 
  match liste_mots with 
    [] -> grid 
  | mot::reste ->
      let new_grid = put_word
          grid
          (List.hd mot.Constraints.domain)
          mot.Constraints.coords_debut
          mot.Constraints.sens
      in
      put_liste new_grid reste 
        
