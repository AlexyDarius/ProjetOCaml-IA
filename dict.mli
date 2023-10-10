(** ouvre un fichier en lecture **)
val ouvre_en_lecture : string -> in_channel

(** lit le fichier et le ferme automatiquement à la fin du fichier **)
val input_option : in_channel -> string option

(** creer le tableau des mots indexé par le nombre de lettres **)
val words : string -> string list array
