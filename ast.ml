

type expression = 
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


and column =
	| COLExpr of expression
	| COLExprId of expression * string

and columnExtends =
	| COLEXTSingle of column
	| COLEXTMany of columnExtends * columnExtends

and projection = 
	| PROJAsterisk
	| PROJColumns of columnExtends

and source = 
	| SOURID of string
	| SOURSQuery of simple_query
	| SOURComma of source * source
	| SOURCrossJoin of source * source
	| SOURJoinOn of source * joinOp * source * condition

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
	| SQUERYSelectFromWhere of projection * source * condition
	| SQUERYSelectAllFromWhere of projection * source * condition
	| SQUERYSelectDistinctFromWhere of projection * source * condition

(* constructors *)

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

let cst_columnExpr e = COLExpr(e)
let cst_columnExprId e s = COLExprId(e,s)

let cst_columnExtendsSingle c = COLEXTSingle(c)
let cst_columnExtendsMany c1 c2 = COLEXTMany(c1,c2)

let cst_projAsterisk = PROJAsterisk
let cst_projColumns c = PROJColumns(c)

let cst_sourId s = SOURID(s)
let cst_sourSQuery sq = SOURSQuery(sq)
let cst_sourComma s1 s2 = SOURComma(s1,s2)
let cst_sourCrossJoin s1 s2 = SOURCrossJoin(s1,s2)
let cst_sourJoinOn s1 op s2 c = SOURJoinOn(s1,op,s2,c)

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
let cst_condAnd c1 c2 = CONDAnd(c1,c2)
let cst_condOr c1 c2 = CONDOr(c1,c2)
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
let cst_squerySelectFromWhere p s c = SQUERYSelectFromWhere(p,s,c)
let cst_squerySelectAllFromWhere p s c = SQUERYSelectAllFromWhere(p,s,c)
let cst_squerySelectDistinctFromWhere p s c = SQUERYSelectDistinctFromWhere(p,s,c)


let rec string_of_query query = match query with
	| SQUERYSelectFrom(proj, src) -> Printf.sprintf "SELECT %s\nFROM %s\n\n"
													   (string_of_projection proj)
													   (string_of_source src)
	| SQUERYSelectFromWhere(proj, src, cond) -> Printf.sprintf "SELECT %s\nFROM %s\nWHERE %s\n\n"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)
	| SQUERYSelectAllFromWhere(proj, src, cond) -> Printf.sprintf "SELECT ALL %s\nFROM %s\nWHERE %s\n\n"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)
	| SQUERYSelectDistinctFromWhere(proj, src, cond) -> Printf.sprintf "SELECT DISTINCT %s\nFROM %s\nWHERE %s\n\n"
													   (string_of_projection proj)
													   (string_of_source src)
													   (string_of_condition cond)


(* string_of section *)

and string_of_projection proj = match proj with
	| PROJAsterisk -> "*"
	| PROJColumns(col_extends) -> string_of_column_extends col_extends


and string_of_column column = match column with
	| COLExpr(expr) -> string_of_expression expr
	| COLExprId(expr, s) -> Printf.sprintf "%s AS %s" (string_of_expression expr) s


and string_of_column_extends col_list = match col_list with
	| COLEXTSingle(c) -> (string_of_column c)
	| COLEXTMany(c1,c2) -> (string_of_column_extends c1)^", "^(string_of_column_extends c2)


and string_of_expression expr = match expr with
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

and string_of_source source = match source with
	| SOURID(str1) -> str1
	| SOURSQuery(squery) -> Printf.sprintf "(%s)" (string_of_query squery)
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


open Value
module R = Relation.Make(Value)

(* End of string_of section *)

(* let eval_condition env cond = match cond with
	| CONDPred(pred1) -> eval_predicate env pred1
	| CONDNotCond(cond1) -> *)

let rec eval_expression env expr = match expr with 
	| EXPRAttribute(str1, str2) ->  let attr1 = Env.find str1 env in
									let attr2 = Env.find str2 env in
									(match (attr1,attr2) with
										| (_,None)  
										| (None,_) -> failwith ""
										| (Some(_),Some(a)) -> (fun t -> R.attribute a t)) 
	| EXPRPar(expr1) -> eval_expression env expr1
	| EXPRInt(_)
	| EXPRFloat(_)	
	| EXPRPlus(_, _)
	| EXPRMinus(_, _) 
	| EXPRAstrisk(_, _)
	| EXPRSlash(_, _)
	| EXPRUMinus(_) 
	| EXPRString(_) 
	| EXPRPPipe(_, _) 
	| EXPRLower(_) 
	| EXPRUpper(_) 
	| EXPRSubString(_,_,_) -> (fun t -> Some(eval_expression_value expr))

and eval_expression_value expr = match expr with
	| EXPRInt(i) -> VInt(i)
	| EXPRFloat(f) -> VFloat(f)
	| EXPRString(s) -> VVChar(s)
	| EXPRPlus(expr1, expr2) -> add (eval_expression_value expr1) (eval_expression_value expr2)
	| EXPRMinus(expr1, expr2) -> sub (eval_expression_value expr1) (eval_expression_value expr2)
	| EXPRAstrisk(expr1, expr2) -> mul (eval_expression_value expr1) (eval_expression_value expr2)
	| EXPRSlash(expr1, expr2) -> div (eval_expression_value expr1) (eval_expression_value expr2)
	| EXPRUMinus(expr1) -> mul (eval_expression_value expr1) (VInt(-1))
	| EXPRPPipe(expr1, expr2) -> concat (eval_expression_value expr1 ) (eval_expression_value expr2)
	| EXPRLower(expr1) -> lower (eval_expression_value expr1)
	| EXPRUpper(expr1) -> upper (eval_expression_value expr1)
	| EXPRSubString(expr1, expr2, expr3) -> sub_string (eval_expression_value expr1) (eval_expression_value expr2) (eval_expression_value expr3)
	| _ -> failwith "eval_expression_value: invalid expression value"

