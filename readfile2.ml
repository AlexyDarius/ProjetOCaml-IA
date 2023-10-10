type case = Vide | Noir | Char of string



let ouvre_en_lecture file =
  try
    open_in file
  with
    exc ->
      Printf.printf "%s ne s'ouvre pas en lecture\n" file;
      raise exc


let input_option = fun ch ->
  try Some (input_line ch) with End_of_file -> close_in ch ; None
        

let char2case char =
  match char with
    "." -> Vide
  | "#" -> Noir
  | h -> Char h

  
let read file =
  let ch_file = ouvre_en_lecture file in
  let line = input_line ch_file in
  let m = String.length line in
  let rec read_rec lines =
    match input_option ch_file with
      None -> List.rev lines
    | Some line ->
        if String.length line <> m then failwith "Test.read : lenght differs";
        read_rec (line::lines)
  in
  let lines = Array.of_list (line::read_rec []) in

  let sep = Str.regexp " +" in
  let lines = Array.map (fun line -> Array.of_list (Str.split sep line)) lines in
    
  Array.map (Array.map char2case) lines




  (*
    
(***Main***)
let () = 
  let grid_file = "grid.txt" in
  let grid = read grid_file in
  (*  print_matrix print_case grid; *)
  print_matrix print_case grid 
  *)

