type sens = Horizontal | Vertical
  
type contraintes = { indice : int; lettre : string; debut : int; fin : int }
      
type intersect = { id_inter : int; coords : int * int }
      
type mot = {
  id : int;
  coords_debut : int * int;
  coords_fin : int * int;
  length : int;
  sens : sens;
  mutable domain : string list;
  constraints : contraintes list;
  intersect : intersect list;
}

      
(** Prend une list de mots, une liste de contraintes et renvoie la liste des mots qui vérifient ces contraintes **)
val words_w_constraints : string list -> contraintes list -> string list


(** creer la liste des mots de la grille avec leurs attributs (id, debut, fin, length,...) **)
val create_list_mots :
  Readfile.case array array -> string list array -> mot list


(** enlève les mots de la liste de mots dans la string list **)
(** enlève les doublons de la grille **)
val delete_duplicate : string list -> mot list -> string list

    
(** Affiche une liste de mots **)
val print_list_mot : mot list -> unit
