type expression
type column
type projection
type source
type joinOp
type condition
type predicate
type value
type simple_query

type expression = 
	| EXPRAttribute of string * string (* id.id *)
	| EXPRPar of expression
	| EXPRInt of int
	| EXPRFloat of float
	| EXPRPlus of expression * expression
	| EXPRMinus of expression * expression
	| EXPRTimes of expression * expression
	| EXPRSlash of expression * expression
	| EXPRUMinus of expression
	| EXPRString of string
	| EXPRPipe of expression * expression
	| EXPRLower of expression
	| EXPRUpper of expression
	| EXPRSubString of expression * expression * expression

type column =
	| COLExpr of expression
	| COLExprId of expression * string

type projection = 
	| PROJAsterisk
	| PROJColumns of column list

type source = 
	| SOURID of string
	| SOURSQuery of simple_query
	| SOURComma of source * source
	| SOURCrossJoin of source * source
	| SOURJoinOn of source * joinOp * source * condition

type joinOp =
	| INNERJOIN
	| JOIN
	| OUTERLEFT
	| OUTERRIGHT
	| OUTERFULL
	| LEFT
	| RIGHT
	| FULL

type condition = 
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

type predicate = 
	| PREDCond of condition
	| PREDEq of expression * expression
	| PREDNeq of expression * expression
	| PREDLt of expression * expression
	| PREDLe of expression * expression
	| PREDGt of expression * expression
	| PREDGe of expression * expression
	| PREDBetween of expression * expression
	| PREDNotBetween of expression * expression
	| PREDNull of expression
	| PREDNotNull of expression

type value = 
	| VInt of int 
	| VFloat of float 
	| VString of string

type simple_query =
	| SQUERYSelectFromWhere of projection * source * condition
	| SQUERYSelectAllFromWhere of projection * source * condition
	| SQUERYSelectDistinctFromWhere of projection * source * condition


(* constructors *)

let cst_exprAttribute s1 s2 = EXPRAttribute(s1,s2)
let cst_exprPar e = EXPRPar(e)
let cst_exprInt i = EXPRInt(i)
let cst_exprPlus e1 e2 = EXPRPlus(e1,e2)
let cst_exprMinus e1 e2 = EXPRMinus(e1,e2)
let cst_exprTimes e1 e2 = EXPRTimes(e1,e2) 
let cst_exprSlash e1 e2 = EXPRSlash(e1,e2)
let cst_exprUMinus e = EXPRUMinus(e)
let cst_exprString s = EXPRString(s) 
let cst_exprPipe e1 e2 = EXPRPipe(e1,e2)
let cst_exprLower e = EXPRLower(e)
let cst_exprUpper e = EXPRUpper(e)
let cst_exprSubString e1 e2 e3 = EXPRSubString(e1,e2,e3)

let cst_columnExpr e = COLExpr(e)
let cst_columnExprId e s = COLExprId(e,s)

let cst_projAsterisk = PROJAsterisk
let cst_projColumns l = PROJColumns(l)

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
let cst_condIsTrue c = CONDIsTrue(c)
let cst_condIsNotTrue c = CONDIsNotTrue(c)
let cst_condIsFalse c = CONDIsFalse(c)
let cst_condIsNotFalse c = CONDIsNotFalse(c)
let cst_condIsUnknow c = CONDIsUnknown(c)
let cst_condIsNotUnknow c = CONDIsNotUnknown(c)

let cst_predCond c = PREDCond(c)
let cst_predEq e1 e2 = PREDEq(e1,e2)
let cst_predNeq e1 e2 = PREDNeq(e1,e2)
let cst_predLt e1 e2 = PREDLt(e1,e2)
let cst_predLe e1 e2 = PREDLe(e1,e2)
let cst_predGt e1 e2 = PREDGt(e1,e2)
let cst_predGe e1 e2 = PREDGe(e1,e2)
let cst_predBetween e1 e2 = PREDBetween(e1,e2)
let cst_predNotBetween e1 e2 = PREDNotBetween(e1,e2)
let cst_predNull e = PREDNull(e)
let cst_predNotNull e = PREDNotNull(e)

let cst_vInt i = VInt(i)
let cst_vFloat f = VFloat(f)
let cst_vString s = VString(s)

let cst_squerySelectFromWhere p s c = SQUERYSelectFromWhere(p,s,c)
let cst_squerySelectAllFromWhere p s c = SQUERYSelectAllFromWhere(p,s,c)
let cst_squerySelectDistinctFromWhere p s c = SQUERYSelectDistinctFromWhere(p,s,c)