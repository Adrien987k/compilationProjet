
open Value
open Sqldate

module R = Relation.Make(Value)

(* constructors *)

type expression = 
	| EXPRId of string
	| EXPRAttribute of string * string (* id.id *)
	| EXPRPar of expression
	| EXPRInt of int
	| EXPRFloat of float
	| EXPRPlus of expression * expression
	| EXPRMinus of expression * expression
	| EXPRAstrisk of expression * expression
	| EXPRSlash of expression * expression
	| EXPRUMinus of expression
	| EXPRString of string
	| EXPRPPipe of expression * expression
	| EXPRLower of expression
	| EXPRUpper of expression
	| EXPRSubString of expression * expression * expression
	| EXPRCaseExpr of expression * whenExprThen
	| EXPRCaseExprElse of expression * whenExprThen * expression
	| EXPRCaseCond of whenCondThen
	| EXPRCaseCondElse of whenCondThen * expression
	| EXPRDate of exprDate
	| EXPRExtract of dateField * exprDate

and exprDate = 
	| DATECurrent
	| DATEDate of date

and whenExprThen = 
	| WHENExprThen of expression * expression
	| WHENExprThenExtends of expression * expression * whenExprThen

and whenCondThen = 
	| WHENCondThen of condition * expression
	| WHENCondThenExtends of condition * expression * whenCondThen

and column =
	| COLExpr of expression
	| COLExprId of expression * string

and columnExtends =
	| COLEXTSingle of column
	| COLEXTMany of column * columnExtends

and projection = 
	| PROJAsterisk
	| PROJColumns of columnExtends

and source = 
	| SOURID of string
	| SOURQuery of query
	| SOURComma of source * source
	| SOURCrossJoin of source * source
	| SOURJoinOn of source * joinOp * source * condition
	| SOURNatural of source * joinOp * source

and joinOp =
	| INNERJOIN
	| JOIN
	| OUTERLEFT
	| OUTERRIGHT
	| OUTERFULL
	| LEFT
	| RIGHT
	| FULL

and condition = 
	| CONDPred of predicate
	| CONDNotCond of condition
	| CONDAnd of condition * condition
	| CONDOr of condition * condition
	| CONDIsTrue of condition
	| CONDIsNotTrue of condition
	| CONDIsFalse of condition
	| CONDIsNotFalse of condition
	| CONDIsUnknown of condition
	| CONDIsNotUnknown of condition

and predicate = 
	| PREDCond of condition
	| PREDEq of expression * expression
	| PREDNeq of expression * expression
	| PREDLt of expression * expression
	| PREDLe of expression * expression
	| PREDGt of expression * expression
	| PREDGe of expression * expression
	| PREDBetween of expression * expression * expression
	| PREDNotBetween of expression * expression * expression
	| PREDNull of expression
	| PREDNotNull of expression

and simple_query =
	| SQUERYSelectFrom of projection * source
	| SQUERYSelectAllFrom of projection * source
	| SQUERYSelectDistinctFrom of projection * source
	| SQUERYSelectFromWhere of projection * source * condition
	| SQUERYSelectAllFromWhere of projection * source * condition
	| SQUERYSelectDistinctFromWhere of projection * source * condition

and query = 
	| QUERYSimple of simple_query
	| QUERYUnion of query * query
	| QUERYUnionAll of query * query
	| QUERYExcept of query * query
	| QUERYExceptAll of query * query
	| QUERYIntersect of query * query
	| QUERYIntersectAll of query * query

let cst_exprId s = EXPRId(s)
let cst_exprAttribute s1 s2 = EXPRAttribute(s1,s2)
let cst_exprPar e = EXPRPar(e)
let cst_exprInt i = EXPRInt(i)
let cst_exprFloat f = EXPRFloat(f)
let cst_exprPlus e1 e2 = EXPRPlus(e1,e2)
let cst_exprMinus e1 e2 = EXPRMinus(e1,e2)
let cst_exprAsterisk e1 e2 = EXPRAstrisk(e1,e2) 
let cst_exprSlash e1 e2 = EXPRSlash(e1,e2)
let cst_exprUMinus e = EXPRUMinus(e)
let cst_exprString s = EXPRString(s) 
let cst_exprPPipe e1 e2 = EXPRPPipe(e1,e2)
let cst_exprLower e = EXPRLower(e)
let cst_exprUpper e = EXPRUpper(e)
let cst_exprSubString e1 e2 e3 = EXPRSubString(e1,e2,e3)

let cst_exprCaseExpr e wet = EXPRCaseExpr(e, wet)
let cst_exprCaseExprElse e1 wet e2 = EXPRCaseExprElse(e1, wet, e2)
let cst_exprCaseCond wct = EXPRCaseCond(wct)
let cst_exprCaseCondElse wct e = EXPRCaseCondElse(wct, e)

let cst_whenExprThen e1 e2 = WHENExprThen(e1, e2)
let cst_whenExprThenExtends e1 e2 wet = WHENExprThenExtends(e1, e2, wet)
let cst_whenCondThen c e = WHENCondThen(c, e)
let cst_whenCondThenExtends c e wct = WHENCondThenExtends(c, e, wct)

let cst_exprDate ed = EXPRDate(ed)
let cst_exprExtract df ed = EXPRExtract(df, ed)

let cst_dateCurrent = DATECurrent
let cst_dateDate d = DATEDate(d)

let cst_columnExpr e = COLExpr(e)
let cst_columnExprId e s = COLExprId(e,s)

let cst_columnExtendsSingle c = COLEXTSingle(c)
let cst_columnExtendsMany c1 c2 = COLEXTMany(c1, c2)

let cst_projAsterisk = PROJAsterisk
let cst_projColumns c = PROJColumns(c)

let cst_sourId s = SOURID(s)
let cst_sourQuery sq = SOURQuery(sq)
let cst_sourComma s1 s2 = SOURComma(s1, s2)
let cst_sourCrossJoin s1 s2 = SOURCrossJoin(s1, s2)
let cst_sourJoinOn s1 op s2 c = SOURJoinOn(s1, op, s2, c)
let cst_sourNaturalJoin s1 op s2 = SOURNatural(s1, op, s2)

let cst_innerjoin = INNERJOIN
let cst_join = JOIN
let cst_outerleft = OUTERLEFT
let cst_outerright = OUTERRIGHT
let cst_outerfull = OUTERFULL
let cst_right = LEFT
let cst_left = RIGHT
let cst_full = FULL

let cst_condPred p = CONDPred(p)
let cst_condNotCond c = CONDNotCond(c)
let cst_condAnd c1 c2 = CONDAnd(c1, c2)
let cst_condOr c1 c2 = CONDOr(c1, c2)
let cst_condIsTrue c = CONDIsTrue(c)
let cst_condIsNotTrue c = CONDIsNotTrue(c)
let cst_condIsFalse c = CONDIsFalse(c)
let cst_condIsNotFalse c = CONDIsNotFalse(c)
let cst_condIsUnknown c = CONDIsUnknown(c)
let cst_condIsNotUnknown c = CONDIsNotUnknown(c)

let cst_predCond c = PREDCond(c)
let cst_predEq e1 e2 = PREDEq(e1,e2)
let cst_predNeq e1 e2 = PREDNeq(e1,e2)
let cst_predLt e1 e2 = PREDLt(e1,e2)
let cst_predLe e1 e2 = PREDLe(e1,e2)
let cst_predGt e1 e2 = PREDGt(e1,e2)
let cst_predGe e1 e2 = PREDGe(e1,e2)
let cst_predBetween e1 e2 e3 = PREDBetween(e1,e2,e3)
let cst_predNotBetween e1 e2 e3 = PREDNotBetween(e1,e2,e3)
let cst_predNull e = PREDNull(e)
let cst_predNotNull e = PREDNotNull(e)

let cst_squerySelectFrom p s = SQUERYSelectFrom(p,s)
let cst_squerySelectAllFrom p s = SQUERYSelectFrom(p,s)
let cst_squerySelectDistinctFrom p s = SQUERYSelectFrom(p,s)
let cst_squerySelectFromWhere p s c = SQUERYSelectFromWhere(p,s,c)
let cst_squerySelectAllFromWhere p s c = SQUERYSelectAllFromWhere(p,s,c)
let cst_squerySelectDistinctFromWhere p s c = SQUERYSelectDistinctFromWhere(p,s,c)

let cst_querySimple sq = QUERYSimple(sq)
let cst_queryUnion q1 q2 = QUERYUnion(q1, q2)
let cst_queryUnionAll q1 q2 = QUERYUnionAll(q1, q2)
let cst_queryExcept q1 q2 = QUERYExcept(q1, q2)
let cst_queryExceptAll q1 q2 = QUERYExceptAll(q1, q2)
let cst_queryIntersect q1 q2 = QUERYIntersect(q1, q2)
let cst_queryIntersectAll q1 q2 = QUERYIntersectAll(q1, q2)

let rec string_of_query query = match query with
	| QUERYSimple sq -> string_of_simple_query sq 
	| QUERYUnion(q1, q2) -> Printf.sprintf "%s\nUNION\n%s"
										 (string_of_query q1)
										 (string_of_query q2)
	| QUERYUnionAll(q1, q2) -> Printf.sprintf "%s\nUNION ALL\n%s"
										 (string_of_query q1)
										 (string_of_query q2)
	| QUERYExcept(q1, q2) -> Printf.sprintf "%s\nEXCEPT\n%s"
										 (string_of_query q1)
										 (string_of_query q2)
	| QUERYExceptAll(q1, q2) -> Printf.sprintf "%s\nEXCEPT ALL\n%s"
										 (string_of_query q1)
										 (string_of_query q2)
	| QUERYIntersect(q1, q2) -> Printf.sprintf "%s\nINTERSECT\n%s"
										 (string_of_query q1)
										 (string_of_query q2)
	| QUERYIntersectAll(q1, q2) -> Printf.sprintf "%s\nINTERSECT ALL\n%s"
										 (string_of_query q1)
										 (string_of_query q2)

and string_of_simple_query squery = match squery with
	| SQUERYSelectFrom(proj, src) -> Printf.sprintf "SELECT %s\nFROM %s"
													   (string_of_projection proj)
													   (string_of_source src)
	| SQUERYSelectAllFrom(proj, src) -> Printf.sprintf "SELECT ALL %s\nFROM %s"
													   (string_of_projection proj)
													   (string_of_source src)
	| SQUERYSelectDistinctFrom(proj, src) -> Printf.sprintf "SELECT DISTINCT %s\nFROM %s"
													   (string_of_projection proj)
													   (string_of_source src)
	| SQUERYSelectFromWhere(proj, src, cond) -> Printf.sprintf "SELECT %s\nFROM %s\nWHERE %s"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)
	| SQUERYSelectAllFromWhere(proj, src, cond) -> Printf.sprintf "SELECT ALL %s\nFROM %s\nWHERE %s"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)
	| SQUERYSelectDistinctFromWhere(proj, src, cond) -> Printf.sprintf "SELECT DISTINCT %s\nFROM %s\nWHERE %s"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)

and string_of_projection proj = match proj with
	| PROJAsterisk -> "*"
	| PROJColumns(col_extends) -> string_of_column_extends col_extends


and string_of_column column = match column with
	| COLExpr(expr) -> string_of_expression expr
	| COLExprId(expr, s) -> Printf.sprintf "%s AS %s" (string_of_expression expr) s


and string_of_column_extends col_list = match col_list with
	| COLEXTSingle(c) -> (string_of_column c)
	| COLEXTMany(c1,c2) -> (string_of_column c1)^", "^(string_of_column_extends c2)


and string_of_expression expr = match expr with
	| EXPRId(str) -> str
	| EXPRAttribute(str1, str2) -> Printf.sprintf "%s.%s" str1 str2
	| EXPRPar(expr1) -> Printf.sprintf "(%s)" (string_of_expression expr)
	| EXPRInt(n) -> Printf.sprintf "%i" n
	| EXPRFloat(r) -> Printf.sprintf "%f" r
	| EXPRPlus(expr1, expr2) -> Printf.sprintf "%s + %s" (string_of_expression expr1)
														 (string_of_expression expr2)
	| EXPRMinus(expr1, expr2) -> Printf.sprintf "%s - %s" (string_of_expression expr1)
														  (string_of_expression expr2)
	| EXPRAstrisk(expr1, expr2) -> Printf.sprintf "%s * %s" (string_of_expression expr1)
														    (string_of_expression expr2)
	| EXPRSlash(expr1, expr2) -> Printf.sprintf "%s / %s" (string_of_expression expr1)
														  (string_of_expression expr2)
	| EXPRUMinus(expr1) -> Printf.sprintf "-%s" (string_of_expression expr1)
	| EXPRString(str1) -> str1
	| EXPRPPipe(expr1, expr2) -> Printf.sprintf "%s || %s" (string_of_expression expr1)
														  (string_of_expression expr2)
	| EXPRLower(expr1) -> Printf.sprintf "LOWER(%s)" (string_of_expression expr1)
	| EXPRUpper(expr1) -> Printf.sprintf "UPPER(%s)" (string_of_expression expr1)
	| EXPRSubString(expr1, expr2, expr3) -> Printf.sprintf "SUBSTRING(%s FROM %s FOR %s)"
														   (string_of_expression expr1)
														   (string_of_expression expr2)
														   (string_of_expression expr3)
	| EXPRCaseExpr(expr1, wet) -> Printf.sprintf "CASE %s %s END" 
														(string_of_expression expr1)
													    (string_of_whenExprThen wet)
	| EXPRCaseExprElse(expr1, wet, expr2) -> Printf.sprintf "CASE %s %s ELSE %s END" 
														(string_of_expression expr1)
													    (string_of_whenExprThen wet)
													    (string_of_expression expr2)
	| EXPRCaseCond(wct) -> Printf.sprintf "CASE %s END" (string_of_whenCondThen wct)
	| EXPRCaseCondElse(wct, expr1) -> Printf.sprintf "CASE %s ELSE %s END" 
													    (string_of_whenCondThen wct)
														(string_of_expression expr1) 
    | EXPRDate(ed) -> string_of_exprDate ed
    | EXPRExtract(df, ed) -> Printf.sprintf "EXTRACT(%s FROM %s)"
    										(string_of_dateField df)
    									    (string_of_exprDate ed)

and string_of_exprDate exprDate = match exprDate with
	| DATECurrent -> string_of_date current_date
	| DATEDate(date) -> string_of_date date

and string_of_whenExprThen wet = match wet with
	| WHENExprThen(expr1, expr2) -> Printf.sprintf "WHEN %s THEN %s" 
												(string_of_expression expr1)
												(string_of_expression expr2)
	| WHENExprThenExtends(expr1, expr2, wet') -> Printf.sprintf "WHEN %s THEN %s %s" 
												(string_of_expression expr1)
												(string_of_expression expr2)
												(string_of_whenExprThen wet')

and string_of_whenCondThen wct = match wct with
	| WHENCondThen(cond1, expr1) -> Printf.sprintf "WHEN %s THEN %s" 
												(string_of_condition cond1)
												(string_of_expression expr1)
	| WHENCondThenExtends(cond1, expr1, wct') ->  Printf.sprintf "WHEN %s THEN %s %s" 
												(string_of_condition cond1)
												(string_of_expression expr1)
												(string_of_whenCondThen wct')

and string_of_source source = match source with
	| SOURID(str1) -> str1
	| SOURQuery(query) -> Printf.sprintf "(%s)" (string_of_query query)
	| SOURComma(src1, src2) -> Printf.sprintf "%s, %s" (string_of_source src1)
													   (string_of_source src2)
	| SOURCrossJoin(src1, src2) -> Printf.sprintf "%s CROSS JOIN %s" 
													   (string_of_source src1)
													   (string_of_source src2)
	| SOURJoinOn(src1, join, src2, cond) -> Printf.sprintf "%s %s %s ON %s"
													 (string_of_source src1)
													 (string_of_joinOp join)
													 (string_of_source src2)
													 (string_of_condition cond)
    | SOURNatural(src1, join, src2) -> Printf.sprintf "%s NATURAL %s %s"
    										(string_of_source src1)
    										(string_of_joinOp join)
    										(string_of_source src2)


and string_of_joinOp join = match join with
	| INNERJOIN -> "INNER JOIN"
	| JOIN -> "JOIN"
	| OUTERLEFT -> "LEFT OUTER JOIN"
	| OUTERRIGHT -> "RIGHT OUTER JOIN"
	| OUTERFULL -> "FULL OUTER JOIN"
	| LEFT -> "LEFT JOIN"
	| RIGHT -> "RIGHT JOIN"
	| FULL -> "FULL JOIN"

and string_of_condition cond = match cond with
	| CONDPred(pred1) -> string_of_predicate pred1
	| CONDNotCond(cond1) -> Printf.sprintf "NOT %s" (string_of_condition cond1)
	| CONDAnd(cond1, cond2) -> Printf.sprintf "%s AND %s" (string_of_condition cond1)
														  (string_of_condition cond2)
	| CONDOr(cond1, cond2) -> Printf.sprintf "%s OR %s" (string_of_condition cond1)
														 (string_of_condition cond2)
	| CONDIsTrue(cond1) -> Printf.sprintf "%s IS TRUE" (string_of_condition cond1)
	| CONDIsNotTrue(cond1) -> Printf.sprintf "%s IS NOT TRUE" (string_of_condition cond1)
	| CONDIsFalse(cond1) -> Printf.sprintf "%s IS FALSE" (string_of_condition cond1)
	| CONDIsNotFalse(cond1) -> Printf.sprintf "%s IS NOT FALSE" (string_of_condition cond1)
	| CONDIsUnknown(cond1) -> Printf.sprintf "%s IS UNKNOW" (string_of_condition cond1)
	| CONDIsNotUnknown(cond1) -> Printf.sprintf "%s IS NOT UNKNOW" (string_of_condition cond1)


and string_of_predicate pred = match pred with 
	| PREDCond(cond1) -> Printf.sprintf "(%s)" (string_of_condition cond1)
	| PREDEq(expr1, expr2) -> Printf.sprintf "%s = %s" (string_of_expression expr1)
													   (string_of_expression expr2)
	| PREDNeq(expr1, expr2) -> Printf.sprintf "%s <> %s" (string_of_expression expr1)
													     (string_of_expression expr2)
	| PREDLt(expr1, expr2) -> Printf.sprintf "%s < %s" (string_of_expression expr1)
													   (string_of_expression expr2)
	| PREDLe(expr1, expr2) -> Printf.sprintf "%s <= %s" (string_of_expression expr1)
													    (string_of_expression expr2)
	| PREDGt(expr1, expr2) -> Printf.sprintf "%s > %s" (string_of_expression expr1)
													   (string_of_expression expr2)
	| PREDGe(expr1, expr2) -> Printf.sprintf "%s >= %s" (string_of_expression expr1)
													    (string_of_expression expr2)
	| PREDBetween(expr1, expr2, expr3) -> Printf.sprintf "%s BETWEEN %s AND %s"
													(string_of_expression expr1)
													(string_of_expression expr2)
													(string_of_expression expr3)
	| PREDNotBetween(expr1, expr2, expr3) -> Printf.sprintf "%s NOT BETWEEN %s AND %s"
													(string_of_expression expr1)
													(string_of_expression expr2)
													(string_of_expression expr3)
	| PREDNull(expr1) -> Printf.sprintf "%s IS NULL"
													(string_of_expression expr1)
	| PREDNotNull(expr1) -> Printf.sprintf "%s IS NOT NULL"
													(string_of_expression expr1)

let rec find_simple_attr att_env str = 													
	let rec find_attr attr_env sattr =
		match attr_env with
		| [] -> None
		| (sattr', attr') :: next ->
			let split = String.split_on_char '.' sattr' in
			match split with
			| table :: attr'' :: [] -> if String.equal sattr attr''
			  						   then Some(sattr')
			  						   else find_attr next sattr
			| _ -> failwith "Erreur invalide attribute" 
    in
	let attr_option = find_attr att_env str in
	begin
	match attr_option with
	| None -> failwith (Printf.sprintf "Error: unknown attribute %s" str)
	| Some att -> att
    end

let rec domain_of_expression r att_env expr = match expr with
	| EXPRId(str) ->  	 begin
						 match Env.find (find_simple_attr att_env str) att_env with
						 | Some(attr') -> R.domain r attr'
						 | None -> failwith (Printf.sprintf "Error unknown attribute: %s" str)
						 end
	| EXPRAttribute(str1, str2) ->  let attr = str1 ^ "." ^ str2 in
									begin
						   			match Env.find attr att_env with
						   			| Some(attr') -> R.domain r attr'
						   			| None -> failwith (Printf.sprintf "Error: unknown attribute: %s"
						   							    attr)
						   		    end
	| EXPRPar(expr1) -> domain_of_expression r att_env expr1
	| EXPRInt(_) -> DInt
	| EXPRFloat(_) -> DFloat
	| EXPRPlus(expr1, expr2) 
	| EXPRMinus(expr1, expr2) 
	| EXPRAstrisk(expr1, expr2)
	| EXPRSlash(expr1, expr2) -> begin
								 match (domain_of_expression r att_env expr1, domain_of_expression r att_env expr2) with
								 | (DInt, DInt) -> DInt
								 | (DInt, DFloat) -> DFloat
								 | (DFloat, DInt) -> DFloat
								 | (DFloat, DFloat) -> DFloat
								 | _ -> failwith (Printf.sprintf "Error: incompatible operation between %s and %s"
												  (string_of_expression expr1) (string_of_expression expr2))
							     end 
	| EXPRUMinus(expr1) -> begin
						   match (domain_of_expression r att_env expr1) with
						   | DInt -> DInt
						   | DFloat -> DFloat
						   | _ -> failwith (Printf.sprintf "Error: cannot apply (-) to %s" (string_of_expression expr1))
						   end
	| EXPRString(_) -> DVChar
	| EXPRPPipe(expr1, expr2) -> begin
								 match (domain_of_expression r att_env expr1, domain_of_expression r att_env expr2) with
								 | (DVChar, _) -> DVChar
								 | _ -> failwith (Printf.sprintf "Error: cannot apply ||, %s or %s is not a VARCHAR"
													(string_of_expression expr1) (string_of_expression expr2))
								 end
	| EXPRLower(expr1)
	| EXPRUpper(expr1) -> begin
						  match domain_of_expression r att_env expr1 with
						  | DVChar -> DVChar 
						  | _ -> failwith (Printf.sprintf "Error: incompatible operation, %s is not a VARCHAR"
						                   (string_of_expression expr1))
						  end
	| EXPRSubString(expr1, expr2, expr3) -> begin 
											match (domain_of_expression r att_env expr1,
											       domain_of_expression r att_env expr2,
											       domain_of_expression r att_env expr3) with
											| (DVChar, DInt, DInt) -> DVChar
											| _ -> failwith (Printf.sprintf "Error: cannot apply substring with arguments %s, %s, %s"
															(string_of_expression expr1) (string_of_expression expr2) (string_of_expression expr3))
											end
	| EXPRDate(_) -> DDate
	| EXPRExtract(_, _) -> DInt 
	| EXPRCaseExpr(expr1, _)
	| EXPRCaseExprElse(expr1, _, _)  
	| EXPRCaseCondElse(_, expr1) -> domain_of_expression r att_env expr1
	| EXPRCaseCond(wct) -> begin
						   match wct with
							| WHENCondThen(_, expr1)
							| WHENCondThenExtends(_, expr1, _) -> domain_of_expression r att_env expr1
						   end
	

let rec is_null att_env expr t = match expr with
	| EXPRId(str) -> begin
					 match Env.find (find_simple_attr att_env str) att_env with
					 | Some(attr') -> begin
					 				  match R.attribute attr' t with
					 				  | Some(v) -> false
					 				  | None -> true
					 				  end
					 | None -> failwith (Printf.sprintf "Error: unknown attrubute: %s" str)
					 end 
	| EXPRAttribute(str1, str2) -> let attr = str1 ^ "." ^ str2 in
								   begin
								   match Env.find attr att_env with
										| Some(att) -> begin
													   match R.attribute att t with
													   | Some(v) -> false
													   | None -> true
													   end
										| None -> 
											failwith (Printf.sprintf "Error: unknown attribute: %s"
																	  attr)
								   end
	| EXPRUpper(expr1) 
	| EXPRLower(expr1) 
	| EXPRUMinus(expr1) 
	| EXPRPar(expr1) -> is_null att_env expr1 t
	| EXPRInt(_) 
	| EXPRFloat(_)
	| EXPRString(_)
	| EXPRDate(_)
	| EXPRExtract(_, _) -> false 
	| EXPRPlus(expr1, expr2) 
	| EXPRAstrisk(expr1, expr2)
	| EXPRSlash(expr1, expr2)  
	| EXPRPPipe(expr1, expr2) 
	| EXPRMinus(expr1, expr2) -> (is_null att_env expr1 t) || (is_null att_env expr2 t) 
	| EXPRSubString(expr1, expr2, expr3) -> (is_null att_env expr1 t) 
										 	|| (is_null att_env expr2 t)
										 	|| (is_null att_env expr3 t)
	| EXPRCaseExpr(_, _)
	| EXPRCaseExprElse(_, _, _)  
	| EXPRCaseCond(_) 
	| EXPRCaseCondElse(_, _) -> false

let is_not_null att_env expr t = not (is_null att_env expr t)

let rec is_unknown att_env cond = match cond with
	| CONDPred(pred1) -> is_unknown_pred att_env pred1
    | CONDAnd(cond1, cond2) -> (fun t -> not ((is_unknown att_env cond1) t)
									     && not ((is_unknown att_env cond2) t))
	| CONDOr(cond1, cond2) -> (fun t -> not ((is_unknown att_env cond1) t)
									     || not ((is_unknown att_env cond2) t))
	| CONDNotCond(cond1)
	| CONDIsTrue(cond1)
	| CONDIsNotTrue(cond1)
	| CONDIsFalse(cond1)
	| CONDIsNotFalse(cond1) -> is_unknown att_env cond1 
	| CONDIsUnknown(cond1)
	| CONDIsNotUnknown(cond1) -> (fun t -> false)

and is_unknown_pred att_env pred = match pred with
	| PREDCond(cond1) -> is_unknown att_env cond1
	| PREDEq(expr1, expr2)
	| PREDNeq(expr1, expr2) 
	| PREDLt(expr1, expr2) 
	| PREDLe(expr1, expr2) 
	| PREDGt(expr1, expr2)
	| PREDGe(expr1, expr2) -> (fun t -> (is_null att_env expr1 t) 
									 || (is_null att_env expr2 t))
	| PREDBetween(expr1, expr2, expr3)
	| PREDNotBetween(expr1, expr2, expr3) -> (fun t -> (is_null att_env expr1 t) 
													|| (is_null att_env expr2 t)
													|| (is_null att_env expr3 t))
	| PREDNull(expr1) -> (fun t -> false)
	| PREDNotNull(expr1) -> (fun t -> false)

and eval_condition att_env cond = match cond with
	| CONDPred(pred1) -> eval_predicate att_env pred1
	| CONDNotCond(cond1) -> (fun t -> not ((eval_condition att_env cond1) t))
    | CONDAnd(cond1, cond2) -> (fun t -> if (eval_condition att_env cond1) t 
    									 then (eval_condition att_env cond2) t
							   			 else false)
	| CONDOr(cond1, cond2) -> (fun t -> if (eval_condition att_env cond1) t then true 
							  else (eval_condition att_env cond2) t)
	| CONDIsTrue(cond1) -> (fun t -> (eval_condition att_env cond1) t)
	| CONDIsNotTrue(cond1) -> (fun t -> not ((eval_condition att_env cond1) t))
	| CONDIsFalse(cond1) -> (fun t -> not ((eval_condition att_env cond1) t))
	| CONDIsNotFalse(cond1) -> (fun t -> (eval_condition att_env cond1) t)
	| CONDIsUnknown(cond1) -> is_unknown att_env cond1
	| CONDIsNotUnknown(cond1) -> (fun t -> not ((is_unknown att_env cond1) t))

and eval_predicate att_env pred =
	let eval_2expr expr1 expr2 op =
		(fun t -> op  
		(match (eval_expression att_env expr1) t with
		| None -> failwith "Error: Syntax error"
		| Some(v1) -> v1)
		(match (eval_expression att_env expr2) t with
		| None -> failwith "Error: Syntax error"
		| Some(v2) -> v2))
	in  
	let eval_3expr expr1 expr2 expr3 op =
		(fun t -> op  
		(match (eval_expression att_env expr1) t with
		| None -> failwith "Error: Syntax error"
		| Some(v1) -> v1)
		(match (eval_expression att_env expr2) t with
		| None -> failwith "Error: Syntax error"
		| Some(v2) -> v2)
		(match (eval_expression att_env expr3) t with
		| None -> failwith "Error: Syntax error"
		| Some(v3) -> v3))
	in
	match pred with
	| PREDCond(cond1) -> eval_condition att_env cond1
	| PREDEq(expr1, expr2) -> eval_2expr expr1 expr2 eq
	| PREDNeq(expr1, expr2) -> eval_2expr expr1 expr2 neq 
	| PREDLt(expr1, expr2) -> eval_2expr expr1 expr2 lt 
	| PREDLe(expr1, expr2) -> eval_2expr expr1 expr2 le 
	| PREDGt(expr1, expr2) -> eval_2expr expr1 expr2 gt 
	| PREDGe(expr1, expr2) -> eval_2expr expr1 expr2 ge
	| PREDBetween(expr1, expr2, expr3) -> eval_3expr expr1 expr2 expr3 between
	| PREDNotBetween(expr1, expr2, expr3) ->  eval_3expr expr1 expr2 expr3 not_between
	| PREDNull(expr1) -> (fun t -> is_null att_env expr1 t)
	| PREDNotNull(expr1) -> (fun t -> is_not_null att_env expr1 t)


and eval_expression att_env expr = 
	let eval_1expr expr1 op = 
		(fun t -> Some(op
		(match (eval_expression att_env expr1) t with
		| None -> failwith "Error: Syntax error"
		| Some(v1) -> v1)))
	in
	let eval_2expr expr1 expr2 op =
		(fun t -> Some(op  
		(match (eval_expression att_env expr1) t with
		| None -> failwith "Error: Syntax error"
		| Some(v1) -> v1)
		(match (eval_expression att_env expr2) t with
		| None -> failwith "Error: Syntax error"
		| Some(v2) -> v2)))
	in  
	let eval_3expr expr1 expr2 expr3 op =
		(fun t -> Some(op  
		(match (eval_expression att_env expr1) t with
		| None -> failwith "Error: Syntax error"
		| Some(v1) -> v1)
		(match (eval_expression att_env expr2) t with
		| None -> failwith "Error: Syntax error"
		| Some(v2) -> v2)
		(match (eval_expression att_env expr3) t with
		| None -> failwith "Error: Syntax error"
		| Some(v3) -> v3)))
	in
    match expr with 
    | EXPRId(str) -> begin
    				 match Env.find (find_simple_attr att_env str) att_env with
    				 | Some(att) -> (fun t -> R.attribute att t)
    				 | None -> failwith (Printf.sprintf "Error: unknown attribute: %s" str)
    				 end
	| EXPRAttribute(str1, str2) -> let attr = str1 ^ "." ^ str2 in
								   begin
								   match Env.find attr att_env with
									| Some(att) -> (fun t -> R.attribute att t)
									| None -> failwith (Printf.sprintf "Error: unknown attribute: %s" attr)   
								   end
	| EXPRPar(expr1) -> eval_expression att_env expr1
	| EXPRInt(i) -> (fun t -> Some(VInt(i)))
	| EXPRFloat(f) -> (fun t -> Some(VFloat(f)))
	| EXPRString(s) -> (fun t -> Some(VVChar(s)))
	| EXPRPlus(expr1, expr2) -> eval_2expr expr1 expr2 add
	| EXPRMinus(expr1, expr2) -> eval_2expr expr1 expr2 sub 
	| EXPRAstrisk(expr1, expr2) -> eval_2expr expr1 expr2 mul
	| EXPRSlash(expr1, expr2) -> eval_2expr expr1 expr2 div
	| EXPRUMinus(expr1) -> eval_2expr expr1 (EXPRInt(-1)) mul 
	| EXPRPPipe(expr1, expr2)  -> eval_2expr expr1 expr2 concat 
	| EXPRSubString(expr1, expr2, expr3) -> eval_3expr expr1 expr2 expr3 sub_string 
	| EXPRLower(expr1) -> eval_1expr expr1 lower 
	| EXPRUpper(expr1) -> eval_1expr expr1 upper 
	| EXPRCaseExpr(expr1, wet) -> (fun t -> 
									match (eval_whenExprThen att_env expr1 wet) t with
									| None -> failwith "Error: case nether match"
									| Some(expr') -> (eval_expression att_env expr') t
								  )
	| EXPRCaseExprElse(expr1, wet, expr2) -> (fun t -> 
											 match (eval_whenExprThen att_env expr1 wet) t with
											 | None -> (eval_expression att_env expr2) t
											 | Some(expr') -> (eval_expression att_env expr') t
											 )
	| EXPRCaseCond(wct) -> (fun t -> match (eval_whenCondExpr att_env wct) t with
									 | Some(expr1) -> (eval_expression att_env expr1) t
									 | None -> failwith "Error: case nether true")
	| EXPRCaseCondElse(wct, expr1) -> (fun t -> match (eval_whenCondExpr att_env wct) t with
									   | Some(expr1') -> (eval_expression att_env expr1') t
									   | None -> (eval_expression att_env expr1) t)
	| EXPRDate(ed) -> (fun t -> Some(VDate(eval_exprDate ed)))
	| EXPRExtract(df, ed) -> (fun t -> Some(VInt(extract_date df (eval_exprDate ed))))

and eval_exprDate ed = match ed with
	| DATEDate d -> d
	| DATECurrent -> current_date

and eval_whenExprThen att_env expr wet =
	let eval_value expropt = match expropt with
	| Some(expr') -> expr'
	| None -> failwith "Error: unknown expression"
	in 
	match wet with
	| WHENExprThen(expr1, expr2) -> (fun t -> 
									if eq 
									   (eval_value ((eval_expression att_env expr) t))
									   (eval_value ((eval_expression att_env expr1) t)) 
	                                then Some(expr2) 
	                                else None 
	                                ) 
	| WHENExprThenExtends(expr1, expr2, wet') -> (fun t -> 
												 if eq 
												 (eval_value ((eval_expression att_env expr) t))
												 (eval_value ((eval_expression att_env expr1) t))
												 then Some(expr2)
												 else (eval_whenExprThen att_env expr wet') t
												)
and eval_whenCondExpr att_env wct = match wct with
	| WHENCondThen(cond1, expr1) -> (fun t -> if ((eval_condition att_env cond1) t) 
											  then Some(expr1)
											  else None )
	| WHENCondThenExtends(cond1, expr1, wct') -> (fun t -> if ((eval_condition att_env cond1) t) 
											  			   then Some(expr1)
											  			   else (eval_whenCondExpr att_env wct') t)   

and eval_source r_env source =
	let rec rename g name = match g with
		  | [] -> [] 
		  | (s, att) :: next -> ((name ^ "." ^ s), att) :: (rename next name)
	in
	let prepare_att_envs r1 r2 g1 g2 =
		let rec count_att g = match g with
			| [] -> 0
			| h :: q -> 1 + count_att q
		  in 
		let nb_att_g1 = count_att g1 in
		let rec change_att g = match g with
		  | [] -> []
		  | (s, att) :: next -> (s, (att + nb_att_g1)) :: (change_att next)
		  in
		let g2 = change_att g2 in
		(g1, g2)
	in
	let join_app s1 s2 join = 
		let source1 = eval_source r_env s1 in
		let source2 = eval_source r_env s2 in
		match source1, source2 with
			| ((r1, g1), (r2, g2)) -> let g1, g2 = prepare_att_envs r1 r2 g1 g2 in
									  (join r1 r2, Env.union g1 g2)
	in
	let join_app_cond cond s1 s2 join = 
		let source1 = eval_source r_env s1 in
		let source2 = eval_source r_env s2 in
		match source1, source2 with
			| ((r1, g1), (r2, g2)) ->
					let g1, g2 = prepare_att_envs r1 r2 g1 g2 in
					let att_env = Env.union g1 g2 in
					let pred_1tuple = eval_condition att_env cond in 
					let pred_2tuple = (fun t1 t2 -> pred_1tuple (R.append t1 t2)) in
					(join pred_2tuple r1 r2, att_env)
	in
	let rec construct_condition_for_natural att_env att_l cond =
			let rec contains_simple_att l v = 
				match l with
				| [] -> (false, "", "") 
				| (table, att) :: next -> if att = v 
									  then (true, table, att)
									  else contains_simple_att next v
			in
			match att_env with
			 | [] -> cond
			 | (s, attr) :: next ->
			 	match String.split_on_char '.' s with
			 	| table :: sattr :: [] -> let contains, res_table, res_sattr = contains_simple_att att_l sattr in   
			 							  if contains
			 							  then construct_condition_for_natural 
			 							  	   next att_l (CONDAnd(cond, CONDPred(PREDEq(
			 							  	   								EXPRAttribute(table, sattr), 
			 							  	   								EXPRAttribute(res_table, res_sattr)))))
			 							  else construct_condition_for_natural
			 							  	   next ([table, sattr] @ att_l) cond
			 	| sattr :: [] -> let contains, res_table, res_sattr = contains_simple_att att_l sattr in
			 					 if contains
			 					 then construct_condition_for_natural
			 					 	  next att_l (CONDAnd(cond, CONDPred(PREDEq(
			 					 	  								EXPRId(sattr),
			 					 	  								EXPRAttribute(res_table, res_sattr)))))
			 					 else construct_condition_for_natural
			 					 	  next (["", sattr] @ att_l) cond
			| _ -> failwith (Printf.sprintf "Error: inalide attribute for natural join")
	in
	let eval_proj_for_natural r att_env =
		let rec contains l v = 
		  match l with
		  | [] -> false
		  | h :: q -> if h = v then true else contains q v
	    in
		let rec reatribute_number_since old_att_env since =
			match old_att_env with
			| [] -> old_att_env
			| (s, attr) :: next -> if attr > since
								   then (s, (attr - 1)) :: (reatribute_number_since next since)
								   else (s, attr) :: (reatribute_number_since next since)
		in
		let rec collect_attributes_for_natural att_env_init att_l att_env_result r = 
			match att_env_init with
			| [] -> ([], att_env_result)
			| (s, attrib) :: next -> 
				let split = String.split_on_char '.' s in
				match split with
				| table :: att :: [] -> if contains att_l att 
										then  let new_env = (Env.remove (table ^ "." ^ att) att_env_result) in
											  let new_env = reatribute_number_since new_env attrib in 
											  (collect_attributes_for_natural next att_l new_env r) 
				 						else 
				 						let result = (collect_attributes_for_natural next ([att] @ att_l) att_env_result) r in
				 						begin
				 						match result with
				 						| (list, att_result') ->  let expr = EXPRAttribute(table, att) in
				 												  let eval_expr = eval_expression att_env expr in
				 												  let domain = domain_of_expression r att_env expr in 
				 												  ([(domain, eval_expr)] @ list, 
				 												  att_result')
				 					    end
				| att :: [] -> if contains att_l att
							   then (collect_attributes_for_natural next att_l
							   		 (Env.remove att att_env_result) r)
							   else  
							   let result = (collect_attributes_for_natural next ([att] @ att_l) att_env_result) r in
							   begin
							   match result with
							   | (list, att_result') -> let expr = EXPRId(att) in
				 									    let eval_expr = eval_expression att_env expr in
				 										let domain = domain_of_expression r att_env expr in
							   							([(domain, eval_expr)] @ list,
															 att_result')
							   end
				| _ -> failwith (Printf.sprintf "Error: inalide attribute for natural join")
		in
		collect_attributes_for_natural att_env ([] : string list) att_env r
	in
	match source with
	| SOURID(str1) -> begin match Env.find str1 r_env with
						| None -> failwith (Printf.sprintf "Error: unknown source : %s" str1)
						| Some(elem) -> begin
										match elem with
										| (r, att_env) -> (r, rename att_env str1)
										end
					  end
	| SOURQuery(query) -> eval_query r_env query
	| SOURComma(src1, src2)
	| SOURCrossJoin(src1, src2) -> join_app src1 src2 R.crossjoin
	| SOURJoinOn(src1, join, src2, cond) ->
		begin
		match join with
			| JOIN  
			| INNERJOIN -> join_app_cond cond src1 src2 R.innerjoin 
			| LEFT 
			| OUTERLEFT -> join_app_cond cond src1 src2 R.leftouterjoin
			| FULL 
			| OUTERFULL -> join_app_cond cond src1 src2 R.fullouterjoin 
			| RIGHT
			| OUTERRIGHT -> join_app_cond cond src2 src1 R.leftouterjoin
		end
	| SOURNatural(src1, join, src2) ->
		let source1 = eval_source r_env src1 in
		let source2 = eval_source r_env src2 in
		let true_predicate = (CONDPred(PREDEq(EXPRInt(0), EXPRInt(0)))) in
		let do_projection att_env rjoin = 
		    begin
		    match join_app_cond
		    (construct_condition_for_natural att_env [] true_predicate)
		    src1 src2 rjoin with
		    | (r, att_env) -> let res_eval_proj = eval_proj_for_natural r att_env in
		   					 begin 
		   					 match res_eval_proj with
		   					 | (list_dom_exrp, new_att_env) ->   
		   					 		(R.projection list_dom_exrp r, new_att_env)
		   					 end
		   end
		in
		match source1, source2 with
			| ((r1, g1), (r2, g2)) ->
					let g1, g2 = prepare_att_envs r1 r2 g1 g2 in
					let att_env = Env.union g1 g2 in  
		begin
		match join with
			| JOIN  
			| INNERJOIN -> do_projection att_env R.innerjoin  				   
			| LEFT 
			| OUTERLEFT -> do_projection att_env R.leftouterjoin 	 	 				   
			| FULL 
			| OUTERFULL -> do_projection att_env R.fullouterjoin 
			| RIGHT
			| OUTERRIGHT -> do_projection att_env R.leftouterjoin
		end
		


and eval_projection r att_env proj =
	let rec collect_attributes att_env = 
		match att_env with
		| [] -> []
		| (s, attrib) :: next -> let split = String.split_on_char '.' s in
								 match split with
								 | table :: att :: [] ->
					             	[COLExpr(EXPRAttribute(table, att))] @ (collect_attributes next)
								 | att :: [] -> 
								 	[COLExpr(EXPRId(att))] @ (collect_attributes next) 
					             | _ -> failwith (Printf.sprintf "Error: invalide attribute")
	in
	let column_list = collect_attributes att_env in
	let rec transform_col_list_in_colExt col_list =
		match col_list with
		| [] -> failwith "Error: No attribute found"
		| col :: [] -> COLEXTSingle(col)
		| col :: next -> COLEXTMany(col, transform_col_list_in_colExt next)
	in
	let col_extends_calculated_with_env = transform_col_list_in_colExt column_list in
	match proj with
	| PROJAsterisk -> eval_column_extends r att_env col_extends_calculated_with_env
	| PROJColumns(col_extends) -> eval_column_extends r att_env col_extends


and eval_column_extends r att_env col_list = 
	let rec eval_column_extends_temp att_env col_list g' res_l nb = 
		match col_list with
		| COLEXTSingle(col) -> begin
								   match col with
								   | COLExpr(expr) -> 
								   				(g', res_l @ [(domain_of_expression r att_env expr, eval_expression att_env expr)])
								   | COLExprId(expr, s) -> 
								   				((Env.add s nb g'),
												 res_l @ [(domain_of_expression r att_env expr, eval_expression att_env expr)])									   							
							   end
		| COLEXTMany(col, col_ext) -> 
			begin
			match col with
			| COLExpr(expr) -> 
					eval_column_extends_temp 
						att_env 
						col_ext 
						g' 
						(res_l @ [(domain_of_expression r att_env expr, eval_expression att_env expr)]) 
						(nb + 1) 
			| COLExprId(expr, s) ->
					eval_column_extends_temp
						att_env 
						col_ext
						(Env.add s nb g')
						(res_l @ [(domain_of_expression r att_env expr, eval_expression att_env expr)])
						(nb + 1)
			end
	in
	eval_column_extends_temp att_env col_list Env.empty [] (0 : R.attribute)




and eval_simple_query r_env squery = 
	let source_proj proj src =
		match eval_source r_env src with
		| (r, att_env) -> let res_eval_proj = eval_projection r att_env proj in
						  match res_eval_proj with
						  | (g', proj1) -> (R.projection proj1 r, g')  
	in
	let source_proj_cond proj src cond = 
		match eval_source r_env src with
			| (r, att_env) -> let res_eval_cond = eval_condition att_env cond in
							  let res_eval_proj = eval_projection r att_env proj in
							  begin
							  match res_eval_proj with
							  | (g', proj1) -> 
							  	    ((R.projection proj1 (R.selection res_eval_cond r)),  g')
							  end
	in
	match squery with
	| SQUERYSelectFrom(proj, src)
	| SQUERYSelectAllFrom(proj, src) -> source_proj proj src
	| SQUERYSelectDistinctFrom(proj, src) -> begin
											 match source_proj proj src with
											 | (r, att_env) -> (R.distinct(r), att_env)
											 end 
	| SQUERYSelectFromWhere(proj, src, cond)
	| SQUERYSelectAllFromWhere(proj, src, cond) -> source_proj_cond proj src cond 
	| SQUERYSelectDistinctFromWhere(proj, src, cond) -> 
				begin
				match source_proj_cond proj src cond with
				| (r, att_env) -> (R.distinct(r), att_env)
				end 

and eval_query r_env query = 
	let app_queries q1 q2 app op_name dist =
		match (eval_query r_env q1), (eval_query r_env q2) with
		| ((r1, g1), (r2, g2)) -> 
			if g1 = g2 then (if dist then (R.distinct (app r1 r2), g1)
							 else (app r1 r2, g1)) 
			else failwith 
			(Printf.sprintf "Error: operation %s cannot be applied. 
							Tables do not have the same attribute environement"
							op_name)
	in
	match query with
	| QUERYSimple sq -> eval_simple_query r_env sq
	| QUERYUnion(q1, q2) -> app_queries q1 q2 R.union "UNION" true
	| QUERYUnionAll(q1, q2) -> app_queries q1 q2 R.union "UNION" false
	| QUERYExcept(q1, q2) -> app_queries q1 q2 R.diff "EXCEPT" true
	| QUERYExceptAll(q1, q2) -> app_queries q1 q2 R.diff "EXCEPT" false
	| QUERYIntersect(q1, q2) -> app_queries q1 q2 R.inter "INTERSECT" true
	| QUERYIntersectAll(q1, q2) -> app_queries q1 q2 R.inter "INTERSECT" false