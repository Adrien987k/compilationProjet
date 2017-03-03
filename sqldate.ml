
open Unix

type date = Date of int * int * int
type dateField = YEAR | MONTH | DAY

let cst_date year month day = Date(year, month, day)

let cst_dateFieldYear = YEAR
let cst_dateFieldMonth = MONTH
let cst_dateFieldDay = DAY 

let date_of_string s = 
	let item_l = String.split_on_char '-' s in
	let rec fillDate date l n = match l with
		| [] -> date
		| h :: q -> match date with 
					| Date(y, m, d) -> if n = 2 then fillDate (Date(int_of_string h, m, d)) q 1
									   else if n = 1 then fillDate (Date(y,int_of_string h, d)) q 0
									   else Date(y, m, int_of_string  h)
	in 
	fillDate (Date(0, 0, 0)) item_l 2 



let string_of_date date = match date with
	| Date(year, month, day) -> Printf.sprintf "%s-%s-%s"
								(string_of_int year)
								(string_of_int month)
								(string_of_int day)

let string_of_dateField dateField = match dateField with
	| YEAR -> "YEAR"
	| MONTH -> "MONTH"
	| DAY -> "DAY"						

let current_date =
	let c_date = gmtime(time ()) in
	Date(1900 + c_date.tm_year, c_date.tm_mon, c_date.tm_mday)

let compare_date date1 date2 predicate = match date1, date2 with
 	| (Date(y1, m1, d1), Date(y2, m2, d2)) -> predicate y1 m1 d1 y2 m2 d2

let eq_date date1 date2 = compare_date date1 date2 
						  (fun y1 m1 d1 y2 m2 d2 -> (y1 = y2) && (m1 = m2) && (d1 = d2)) 
let neq_date date1 date2 = not (eq_date date1 date2) 


let lt_date date1 date2 = compare_date date1 date2 (fun y1 m1 d1 y2 m2 d2 -> 
												    if y1 < y2 then true
													else if y1 > y2 then false 
													else if m1 < m2 then true 
													else if m1 > m2 then false 
													else if d1 < d2 then true
													else if d1 > d2 then false 
													else false)   
let le_date date1 date2 = (lt_date date1 date2) || (eq_date date1 date2)
let gt_date date1 date2 = not (le_date date1 date2)
let ge_date date1 date2 = not (gt_date date1 date2)

let between_date date1 date2 date3 = (ge_date date2 date1) && (le_date date2 date3)

let extract_date df date = match df, date with
	| YEAR, Date(y, m, d) -> y
	| MONTH, Date(y, m, d) -> m
	| DAY, Date(y, m, d) -> d 
