type mot = {mutable words : string list ; mutable previous_words : string list ; constraints : (int*string*int*int) list ; id : int ; sens : string ; debut : int*int ;
            fin : int*int ; length : int ; mutable intersect : (int * (int*int)) list} (* pour les contraintes: indice, lettre, début, fin *)
            
type state = {
  liste_words: mot list; 
  full : int; (* condition d'arrêt *)
}

(* Vérification de la satisfaction des contraintes *)
let is_ok state =

(* inutile comme ça*)
let rec solve state =
  let rec loop i =
    if i = state.full (* si le critère d'arret est vérifié, on s'arrête *)
    then false
    else if is_ok state i
      then begin
        state.word.(i) <- w
        if solve state

let state = "s"

(* Fonction qui permet de prendre un mot pas encore rempli (suite de ... en regexp) *)
(* Prendre un mot = récupérer son id *)
(* Amélioration possible: renger la liste de mots selon un certain ordre (ex: les mots avec le plus de contraintes d'abord) *)
let get_unknown = fun state -> 
  let list_mots = state.word in 
  let rec get_id = fun l -> 
    match l with 
      [] -> None (* il ne reste plus aucun mot inconnu *)
    | mot::reste -> if List.length (mot.words) > 1 (* si le mot n'est pas déjà affecté *)
                    then Some mot.id (* Some pour pouvoir avoir le même type que None dans l'autre cas *)
                    else get_id reste
  in get_id list_mots

(* ? *)
let recherche = fun s -> ()

(* Fonction qui renvoie la liste des mots possibles pour un mot pas encore rempli *)
let values = fun word dico ->
  match_string dico.(word.constraints.length) word.constraints.regexp

(* Fonction qui remplit le mot qui correspond à l'id avec var *)
let assign = fun state var id ->
  let list_mots = state.word in 
  let rec assign_mot = fun l -> 
    match l with 
      [] -> raise "Erreur: id non présent dans la liste\n"
    | mot::reste -> if mot.id = id 
                    then mot.words <- [var] 
                      (*in state.word@[assigned_word] (* on a ajouté le mot assigné à la liste de mots de state *)
                      (* IL NE FAUT PAS OUBLIER DE SUPPRIMER LE MOT INITIALEMENT PRESENT DANS LA LISTE *) *)
                    else assign_mot reste
  in assign_mot list_mots  


(* Fonction auxiliaire de propagate *)
let rec aux list_mots_non_assigne word mot id_inter x y =
  match list_mots_non_assigne with
    [] -> [] 
  | mot_inter::t ->
      begin
        if Test.(mot_inter.id) = id_inter
        then
          if mot_inter.sens = "horizontal"
          then
            let _ ,yd = mot_inter.debut in
            let xd,_ = Test.(mot.debut) in
            let lettre = String.make 1 (word.[x-xd]) in
            let _, yf = mot_inter.fin in
            {mot_inter with words = Dict.constraints mot_inter.words [(y, lettre, yd, yf)]}::(aux t word mot id_inter x y)  
          else if mot_inter.sens = "vertical"
          then
            let indice = x in
            let xd, _ = mot_inter.debut in
            let _, yd = mot.debut in
            let lettre = String.make 1 (word.[y-yd]) in
            let xf, _ = mot_inter.fin in
            {mot_inter with words = Dict.constraints mot_inter.words [(x, lettre, xd, xf)]}::(aux t word mot id_inter x y)
          else failwith "erreur"
        else mot_inter::(aux t word mot id_inter x y) 
      end

let get_non_assign = fun state -> 
  let liste_mot = state.liste_words in
  let rec construct_liste = mots liste_sortie ->
    match mots with 
      [] -> liste_sortie
    | mot::reste -> if List.length (mot.words) > 1
                    then construct_liste reste (mot::liste_sortie)
                    else construct_liste reste liste_sortie
  in construct_liste liste_mot []
    
(* Propage la nouvelle liste de contraintes *)
let propagate = fun state new_word type_mot mot_intersect ->
  let list_mots_non_assignee = get_non_assign in 
  match mot_intersect with
    [] -> list_mots_non_assigne
  | (id_inter, (x,y))::t ->
      add_constraints (aux list_mots_non_assigne word type_mot id_inter x y) word type_mot t

type option = None | Some of qqchose -> ()


let rec bt = fun state ->
  match get_unknown state with
    None -> Some state (* il ne reste aucun cas mot non attribué: on a fini *)
    |Some var -> recherche var

    (*for word in values(var)*)
    (*let rec values = fun fvalues var ->
      match values var with*)
        
      let nstate = assign state var in
        match (propagate nstate var mot mot.intersect) with
          None -> echec = ()
          |Some newnstate -> match bt newnstate with
            None -> ()
            |Some state -> Some state
    None

(*struct de données S: {S with state = nstate}*)
