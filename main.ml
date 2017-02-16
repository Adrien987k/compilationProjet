open Value 

module R =
struct

  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)

  (* Fonctions d'agrégation (à compléter...) *)
  let sum dist =
    fun a r -> fold dist (fun acc v -> match acc with None -> Some v | Some v' -> Some (Value.add v' v)) None a r

end

let _ =
  let vin_att, vin = R.from_file "vin.csv" '|' in
  let _, viticulteur = R.from_file "viticulteur.csv" '|' in
  let _, client = R.from_file "client.csv" '|' in
  let _, commande = R.from_file "commande.csv" '|' in 
  let r1 = R.selection (fun t -> match R.attribute (List.assoc "Région" vin_att) t with Some (VVChar "Bordeaux") -> true | _ -> false) vin in
  let r2 = R.selection (fun t -> match R.attribute 4 t with Some (VVChar "Alsace") -> true | _ -> false) vin in
  let r3 = R.projection [ DInt, R.attribute 0 ; DInt, R.attribute 2 ; DInt, R.attribute 4 ] commande in
  let r4 = R.projection [ DInt, R.attribute 3 ] vin in
  let r4' = R.distinct r4 in
  let r5 = R.union viticulteur client in
  let r5' = R.distinct r5 in
  let r6 = R.inter viticulteur client in
  let r7 = R.diff viticulteur client in
  let r8 = R.diff client viticulteur in
  let r9 = R.crossjoin client commande in
  let r10 = R.crossjoin viticulteur vin in
  let r11 = R.innerjoin (fun v c -> R.attribute 2 c = R.attribute 0 v && match R.attribute 4 c with Some (VInt q) when q >= 30 -> true | _ -> false) vin commande in
  let r12 = R.innerjoin (fun vit vin -> R.attribute 3 vit = R.attribute 4 vin) viticulteur vin in
  let r13 = R.innerjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r14 = R.leftouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r15 = R.fullouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r16 = R.fullouterjoin (fun v c -> R.attribute 2 c = R.attribute 0 v) vin commande in
  let r17 = R.aggregate [ 0 ] [ DInt, R.sum false 9 ] r16 in
  let _ = R.print '|' [] r17 in
  ()

