(** Type d'une case: Noire, Vide ou contenant un caractère **)
type case = Vide | Noir | Char of string

(** Lit le fichier et le transforme en un tableau de tableau de cases **)
val read : string -> case array array

