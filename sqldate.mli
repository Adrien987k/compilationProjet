

type date 
type dateField

val cst_date: int -> int -> int -> date
val cst_dateFieldYear: dateField
val cst_dateFieldMonth: dateField
val cst_dateFieldDay: dateField

val string_of_date: date -> string 
val date_of_string: string -> date
val string_of_dateField: dateField -> string

val current_date: date

val eq_date: date -> date -> bool
val neq_date: date -> date -> bool
val lt_date: date -> date -> bool
val le_date: date -> date -> bool
val gt_date: date -> date -> bool
val ge_date: date -> date -> bool

val between_date: date -> date -> date -> bool 

val extract_date: dateField -> date -> int