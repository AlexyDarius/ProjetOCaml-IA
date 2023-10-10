type case = Vide | Noir | Char of string

        
(* transforme une case en un format utilisable pour les expressions régulières *)
let char2case char =
  match char with
    "." -> Vide
  | "#" -> Noir
  | h -> Char (String.lowercase_ascii h)

        
(* lit le fichier et le transforme en une matrice de cases *)
let read file =
  let ch_file = Dict.ouvre_en_lecture file in
  let line = input_line ch_file in
  let m = String.length line in
  let rec read_rec lines =
    match Dict.input_option ch_file with
      None -> List.rev lines   (* cas fin du fichier pour remettre la liste dans l'ordre *)
    | Some line ->
        if String.length line <> m then failwith "Test.read : lenght differs";
        read_rec (line::lines)
  in
  let lines = Array.of_list (line::read_rec []) in
  let sep = Str.regexp " +" in
  let lines = Array.map (fun line -> Array.of_list (Str.split sep line)) lines in
    
  Array.map (Array.map char2case) lines
