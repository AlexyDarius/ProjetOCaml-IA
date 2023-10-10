(** affiche une matrice de cases **)
val print_matrix : Readfile.case array array -> unit


(** renvoie une nouvelle grille avec les mots mis au bon endroit **)
val put_liste :
  Readfile.case array array ->
  Constraints.mot list -> Readfile.case array array
