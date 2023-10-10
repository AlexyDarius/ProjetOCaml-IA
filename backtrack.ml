(* trie la liste des mots en foncion du nombre d'intersection et de
 la longueur des domaines *)
let ordonne_mots_static = fun liste_mots -> 
  let compare_intersect = fun mot1 mot2 -> 
    let ordre = Constraints.(-Stdlib.compare (List.length mot1.intersect) (List.length mot2.intersect)) in
    if ordre = 0 
    then (Stdlib.compare (List.length mot1.domain) (List.length mot2.domain)) 
    else ordre
  in 
  List.sort compare_intersect liste_mots

    
(* trie la liste des mots en fonction de la longueur des domaines et
 du nombre d'intersection *) 
let ordonne_mots_dynamic list_mots =
  let compare_domain mot1 mot2 =
    let ordre = compare (List.length mot1.Constraints.domain) (List.length mot2.Constraints.domain) in
    if ordre = 0
    then -compare (List.length mot1.Constraints.intersect) (List.length mot2.Constraints.intersect)
    else ordre
  in
  List.sort compare_domain list_mots 
    


(* actualise le domaine des mots qui intersectent avec le mot 'mot' *)
let rec contraintes_lettres list_mots_non_assigne word mot id_inter x_inter y_inter =
  match list_mots_non_assigne with
    [] -> [] 
  | mot_inter::t ->
      begin
        if mot_inter.Constraints.id = id_inter
        then
          match mot_inter.sens with
            Constraints.Horizontal ->
              let _ ,yd = mot_inter.Constraints.coords_debut in
              let xd,_ = mot.Constraints.coords_debut in                       
              let lettre = String.make 1 (word.[x_inter-xd]) in              
              let _, yf = mot_inter.Constraints.coords_fin in
              {mot_inter with
               domain = Constraints.words_w_constraints
                 mot_inter.domain
                 [{indice = y_inter ; lettre = lettre ; debut = yd ; fin = yf}]}::(contraintes_lettres t word mot id_inter x_inter y_inter)
                                                                                    (* nouveau domaine avec une contrainte en plus*)

          | Constraints.Vertical ->
              let xd, _ = mot_inter.Constraints.coords_debut in
              let _, yd = mot.Constraints.coords_debut in 
              let lettre = String.make 1 (word.[y_inter-yd]) in
              let xf, _ = mot_inter.Constraints.coords_fin in
              {mot_inter with
               domain = Constraints.words_w_constraints
                 mot_inter.domain
                 [{indice = x_inter ; lettre = lettre ; debut = xd ; fin = xf}]}::(contraintes_lettres t word mot id_inter x_inter y_inter)
        else mot_inter::(contraintes_lettres t word mot id_inter x_inter y_inter) 
      end

(* ajoute des contraintes supplémentaires aux mots de list_mots_non_assigne
 appartenant à mot_intersect du fait de
 l'assignation du mot 'words' à 'mot' de type mot *)        
let rec add_constraints list_mots_non_assigne word mot mot_intersect =
  match mot_intersect with
    [] -> list_mots_non_assigne
  | intersect::t ->
      let x_inter, y_inter = intersect.Constraints.coords in
      let id_inter = intersect.Constraints.id_inter in
      add_constraints (contraintes_lettres list_mots_non_assigne word mot id_inter x_inter y_inter) word mot t

        
(* applique l'algorithme de backtrack sur une liste de mots *)  
let backtrack list_mots_non_assigne =
  let rec rec_backtrack list_mots_assigne list_mots_non_assigne = 
    match list_mots_non_assigne with
      [] -> Some list_mots_assigne
    | mot::tail ->
        
        let possible_mots = Constraints.delete_duplicate mot.Constraints.domain list_mots_assigne in 
        (* on enlève les mots déjà assigne dans la liste des mots possible *)
             
        let rec put_word possible_mots =
          match possible_mots with
            [] -> None
          | word::t ->              
              let new_list_mots = add_constraints tail word mot mot.Constraints.intersect in
              (* on update le domaine possible pour
                 les mots de list_mots_non_assigne (tail) en considérant
                 qu'on met le mot words *)
              
              let new_list_mots = ordonne_mots_dynamic new_list_mots in
              (* on trie la nouvelle liste *)
            
              let cond = List.exists (fun x -> if x.Constraints.domain = [] then true else false) new_list_mots in
              (* let cond = true in *)
            
              let result = 
                if cond
                then
                  None  
                else
                  rec_backtrack (Constraints.{mot with domain = [word]}::list_mots_assigne) new_list_mots        
              in
              
              match result with
                None ->  put_word t
              | Some s -> Some s
        in
        put_word possible_mots
  in
  rec_backtrack [] list_mots_non_assigne
