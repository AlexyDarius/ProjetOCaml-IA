(** trie la liste des mots en foncion du nombre d'intersection et de la longueur des domaines **)
val ordonne_mots_static : Constraints.mot list -> Constraints.mot list

    
(** trie la liste des mots en fonction de la longueur des domaines et du nombre d'intersection **) 
val ordonne_mots_dynamic : Constraints.mot list -> Constraints.mot list


(** applique l'algorithme de backtrack sur une liste de mots **)
val backtrack : Constraints.mot list -> Constraints.mot list option
