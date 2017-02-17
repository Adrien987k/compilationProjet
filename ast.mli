type expression
type column
type projection
type source
type joinOp
type condition
type predicate
type value
type simple_query

val cst_exprAttribute: string -> string -> expression
val cst_exprPar: expression -> expression
val cst_exprInt: int -> expression
val cst_exprPlus: expression -> expression -> expression
val cst_exprMinus: expression -> expression -> expression
val cst_exprTimes: expression -> expression -> expression
val cst_exprSlash: expression -> expression -> expression
val cst_exprUMinus: expression -> expression
val cst_exprString: string -> expression
val cst_exprPipe: expression -> expression
val cst_exprLower: expression -> expression
val cst_exprUpper: expression -> expression
val cst_exprSubString: expression -> expression -> expression -> expression

val cst_columnExpr: expression -> column
val cst_columnExprId: expression -> string -> column

val cst_projAsterisk: projection
val cst_projColumns: column list -> projection

val cst_innerjoin: joinOp
val cst_join: joinOp
val cst_outerleft: joinOp
val cst_outerright: joinOp
val cst_outerfull: joinOp
val cst_right: joinOp
val cst_left: joinOp
val cst_full: joinOp

val cst_condPred: predicate -> condition
val cst_condNotCond: condition -> condition
val cst_condAnd: condition -> condition
val cst_condIsTrue: condition -> condition
val cst_condIsNotTrue: condition -> condition
val cst_condIsFalse: condition -> condition
val cst_condIsNotFalse: condition -> condition
val cst_condIsUnknow: condition -> condition
val cst_condIsNotUnknow: condition -> condition

val cst_predCond: expression -> expression -> predicate
val cst_predEq: expression -> expression -> predicate
val cst_predNeq: expression -> expression -> predicate
val cst_predLt: expression -> expression -> predicate
val cst_predLe: expression -> expression -> predicate
val cst_predGt: expression -> expression -> predicate
val cst_predGe: expression -> expression -> predicate
val cst_predBetween: expression -> expression -> predicate
val cst_predNotBetween: expression -> expression -> predicate
val cst_predNull: expression -> predicate
val cst_predNotNull: expression -> predicate

val cst_vInt: int -> value
val cst_vFloat: float -> value
val cst_vString: string -> value

val cst_squerySelectFromWhere: projection -> source -> condition -> simple_query
val cst_squerySelectAllFromWhere: projection -> source -> condition -> simple_query
val cst_squerySelectDistinctFromWhere: projection -> source -> condition -> simple_query