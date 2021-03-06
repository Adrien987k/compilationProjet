(* Module pour la représentation et la manipulation des valeurs atomiques
 *
 * Ce module respecte la signature Relation.DATA et peut donc être utilisé
 * en argument de Relation.Make
 *)

(* Définition des types relatant des domaines et des valeurs atomiques manipulables *)

open Sqldate

type domain =
  | DInt
  | DFloat
  | DVChar
  | DDate

type value =
  | NULL
  | VInt   of int
  | VFloat of float
  | VVChar of string
  | VDate of date


(* Fonctions de conversion entre chaînes de caractères et valeurs/domaines (utilisées dans l'import/export des CSV) *)

let domain_of_string s =
  match s with
  | "INT" -> DInt
  | "FLOAT" -> DFloat
  | "VARCHAR" -> DVChar
  | "DATE" -> DDate
  | _ -> failwith (Printf.sprintf "Value: domain_of_string: unknown domain: '%s'" s)

let string_of_domain d =
  match d with
  | DInt -> "INT"
  | DFloat -> "FLOAT"
  | DVChar -> "VARCHAR"
  | DDate -> "DATE"

let value_of_string d =
  match d with
  | DInt -> (fun s -> if s = "" then NULL else VInt (int_of_string s))
  | DFloat -> (fun s -> if s = "" then NULL else VFloat (float_of_string s))
  | DVChar -> (fun s -> if s = "" then NULL else VVChar s)
  | DDate -> (fun s -> if s = "" then NULL else VDate(date_of_string s))

let string_of_value v =
  match v with
  | NULL -> "NULL"
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VVChar s -> if s = "" then "NULL" else s
  | VDate d -> string_of_date d

(* Fonctions de conversion et de vérification d'appartenance d'une valeur à un domaine *)

let domain_of_value v =
  match v with
  | NULL -> failwith "Error: cannot evaluate domain of NULL"
  | VInt _ -> DInt
  | VFloat _ -> DFloat
  | VVChar _ -> DVChar
  | VDate _ -> DDate

let domain d v =
  match d, v with
  | DInt, VInt _
  | DFloat, VFloat _
  | DVChar, VVChar _
  | DDate, VDate _ -> true
  | _ -> false

let to_domain d =
  match d with
  | DInt -> (function
    | VInt i -> VInt i
    | VFloat f -> VInt (int_of_float f)
    | VVChar s -> (try VInt (int_of_string s) with Failure _ -> VInt 0)
    | VDate d -> failwith "Cannot convert DATE to INT"
    | NULL -> NULL
  )
  | DFloat -> (function
    | VInt i -> VFloat (float_of_int i)
    | VFloat f -> VFloat f
    | VVChar s -> (try VFloat (float_of_string s) with Failure _ -> VFloat 0.)
    | VDate _ -> failwith "Cannot convert DATE to FLOAT"
    | NULL -> NULL
  )
  | DVChar -> (function
    | VInt i -> VVChar (string_of_int i)
    | VFloat f -> VVChar (string_of_float f)
    | VVChar s -> VVChar s
    | VDate d -> VVChar(string_of_date d)
    | NULL -> NULL
  )
  | DDate -> (function
    | VInt _ -> failwith "Cannot convert INT to DATE"
    | VFloat _ -> failwith "Cannot convert FLOAT to DATE"
    | VVChar s -> VDate(date_of_string s)
    | VDate d -> VDate d 
    | NULL -> NULL
  )

(* Fonction spécifique de manipulation des valeurs (comparaison, addition, concaténation, etc.) *)

let app_arithm v1 v2 op_name op1 op2 =
	match (v1,v2) with
	| VInt i1, VInt i2 -> VInt (op1 i1 i2)
	| VInt i1, VFloat f1 -> VFloat(op2 (float_of_int i1) f1)
  | VFloat f1, VInt i1 -> VFloat(op2  f1 (float_of_int i1))
	| VFloat f1, VFloat f2 -> VFloat (op2 f1 f2)
  | NULL, _
  | _, NULL -> NULL
  | _ -> failwith (Printf.sprintf "Error: invalid operation '%s %s %s'" 
                                  (string_of_value v1)
                                  (op_name) 
                                  (string_of_value v2))

let add v1 v2 = app_arithm v1 v2 "+" (+) (+.)
let sub v1 v2 = app_arithm v1 v2 "-" (-) (-.)
let mul v1 v2 = app_arithm v1 v2 "*" ( * ) ( *. )
let div v1 v2 = app_arithm v1 v2 "/" (/) (/.)


let concat v1 v2 =
  match (v1, v2) with
  | VVChar s1, VVChar s2 -> VVChar (s1 ^ s2)
  | VVChar s1, VInt i -> VVChar (s1 ^ (string_of_int i))
  | VVChar s1, VFloat f -> VVChar (s1 ^ (string_of_float f))
  | VInt i, VVChar s1 -> failwith (Printf.sprintf "Error: concat: '%s || %s' Try to invert the order." 
                                                  (string_of_value v1)
                                                  (string_of_value v2))
  | VFloat f , VVChar s1 -> failwith (Printf.sprintf "Error: concat: '%s || %s' Try to invert the order."
                                                     (string_of_value v1)
                                                     (string_of_value v2))
  | NULL, _ 
  | _, NULL -> NULL
  | _ -> failwith (Printf.sprintf "Error: concat: '%s || %s'" (string_of_value v1) (string_of_value v2))

let app_string s1 f e =
  match s1 with
  | VVChar s -> VVChar( (f s) ) 
  | _ -> failwith (Printf.sprintf "Error: %s: %s is not a string" e (string_of_value s1))


let lower s = app_string s String.lowercase_ascii ("LOWER")
let upper s = app_string s String.uppercase_ascii ("UPPER")

let sub_string s i1 i2 =
  match (s,i1,i2) with
  | VVChar s, VInt i1, VInt i2 -> VVChar(String.sub s i1 i2)
  | _ -> failwith (Printf.sprintf "Error: SUBSTRING: %s %s %s" 
                                  (string_of_value s)
                                  (string_of_value i2)
                                  (string_of_value i1))

let app_bool v1 v2 e op_i op_f op_s op_date = match (v1, v2) with
  | VInt i1, VInt i2 -> op_i i1 i2
  | VInt i1, VFloat f1 -> op_f (float_of_int i1) f1
  | VFloat f1, VInt i1 -> op_f f1 (float_of_int i1)
  | VFloat f1, VFloat f2 -> op_f f1 f2
  | VVChar s1, VVChar s2 -> op_s s1 s2
  | VDate d1, VDate d2 -> op_date d1 d2
  | _ -> failwith (Printf.sprintf "Error: try to compare values %s %s with %s"
                                    (string_of_value v1) (string_of_value v2) e)

let eq v1 v2 = app_bool v1 v2 ("=") (=) (=) (String.equal) eq_date
let neq v1 v2 = app_bool v1 v2 ("<>") (<>) (<>) (fun s1 s2 -> not (String.equal s1 s2)) neq_date
let lt v1 v2 = app_bool v1 v2 ("<") (<) (<) (fun s1 s2 -> (String.compare s1 s2) < 0) lt_date
let le v1 v2 = app_bool v1 v2 ("<=") (>=) (>=) (fun s1 s2 -> (String.compare s1 s2) <= 0) le_date
let gt v1 v2 = app_bool v1 v2 (">") (>) (>) (fun s1 s2 -> (String.compare s1 s2) > 0) gt_date
let ge v1 v2 = app_bool v1 v2 (">=") (>=) (>=) (fun s1 s2 -> (String.compare s1 s2) <= 0) ge_date

let between v1 v2 v3 = match (v1, v2, v3) with
  | VInt i1, VInt i2, VInt i3 -> (i1 >= i2) && (i1 <= i3)
  | VFloat f1, VFloat f2, VFloat f3 -> (f1 >= f2) && (f1 <= f3)
  | VVChar s1, VVChar s2, VVChar s3 -> ((String.compare s1 s2) >= 0) &&
                                       ((String.compare s1 s3) <= 0) 
  | VDate d1, VDate d2, VDate d3 -> between_date d2 d1 d3                                     
  | _ -> failwith "Error: try to apply between with invalid arguments"

let not_between v1 v2 v3 = not (between v1 v2 v3)
