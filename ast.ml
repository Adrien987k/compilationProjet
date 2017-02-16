type simple_query
type projection
type column

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

