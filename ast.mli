
open Sqldate

module R : Relation.S

type expression
type column
type columnExtends
type projection
type source
type joinOp
type condition
type predicate
type simple_query
type whenExprThen
type whenCondThen
type exprDate

val cst_exprAttribute: string -> string -> expression
val cst_exprPar: expression -> expression
val cst_exprInt: int -> expression
val cst_exprFloat: float -> expression
val cst_exprPlus: expression -> expression -> expression
val cst_exprMinus: expression -> expression -> expression
val cst_exprAsterisk: expression -> expression -> expression
val cst_exprSlash: expression -> expression -> expression
val cst_exprUMinus: expression -> expression
val cst_exprString: string -> expression
val cst_exprPPipe: expression -> expression -> expression
val cst_exprLower: expression -> expression
val cst_exprUpper: expression -> expression
val cst_exprSubString: expression -> expression -> expression -> expression
val cst_exprCaseExpr: expression -> whenExprThen -> expression
val cst_exprCaseExprElse: expression -> whenExprThen -> expression -> expression
val cst_exprCaseCond: whenCondThen -> expression
val cst_exprCaseCondElse: whenCondThen -> expression -> expression
val cst_exprDate: exprDate -> expression
val cst_exprExtract: dateField -> exprDate -> expression

val cst_dateCurrent: exprDate
val cst_dateDate: date -> exprDate

val cst_whenExprThen: expression -> expression -> whenExprThen
val cst_whenExprThenExtends: expression -> expression -> whenExprThen -> whenExprThen
val cst_whenCondThen: condition -> expression -> whenCondThen
val cst_whenCondThenExtends: condition -> expression -> whenCondThen -> whenCondThen

val cst_columnExpr: expression -> column
val cst_columnExprId: expression -> string -> column

val cst_columnExtendsSingle: column -> columnExtends
val cst_columnExtendsMany: column -> columnExtends -> columnExtends

val cst_projAsterisk: projection
val cst_projColumns: columnExtends -> projection

val cst_sourId: string -> source
val cst_sourSQuery: simple_query -> source
val cst_sourComma: source -> source -> source
val cst_sourCrossJoin: source -> source -> source
val cst_sourJoinOn: source -> joinOp -> source -> condition -> source

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
val cst_condAnd: condition -> condition -> condition
val cst_condOr: condition -> condition -> condition
val cst_condIsTrue: condition -> condition
val cst_condIsNotTrue: condition -> condition
val cst_condIsFalse: condition -> condition
val cst_condIsNotFalse: condition -> condition
val cst_condIsUnknown: condition -> condition
val cst_condIsNotUnknown: condition -> condition

val cst_predCond: condition -> predicate
val cst_predEq: expression -> expression -> predicate
val cst_predNeq: expression -> expression -> predicate
val cst_predLt: expression -> expression -> predicate
val cst_predLe: expression -> expression -> predicate
val cst_predGt: expression -> expression -> predicate
val cst_predGe: expression -> expression -> predicate
val cst_predBetween: expression -> expression -> expression -> predicate
val cst_predNotBetween: expression -> expression -> expression -> predicate
val cst_predNull: expression -> predicate
val cst_predNotNull: expression -> predicate

val cst_squerySelectFrom: projection -> source -> simple_query
val cst_squerySelectAllFrom: projection -> source -> simple_query
val cst_squerySelectDistinctFrom: projection -> source -> simple_query
val cst_squerySelectFromWhere: projection -> source -> condition -> simple_query
val cst_squerySelectAllFromWhere: projection -> source -> condition -> simple_query
val cst_squerySelectDistinctFromWhere: projection -> source -> condition -> simple_query

val string_of_query: simple_query -> string
val string_of_exprDate: exprDate -> string

(*val eval_condition: 'a Env.env -> condition -> ('a -> bool)
val eval_predicate: 'a Env.env -> predicate -> ('a -> bool) *)
(* val eval_expression: 'a Env.env -> expression -> ('b -> 'c) *)
val eval_query: (R.relation * R.attribute Env.env) Env.env -> simple_query -> (R.relation * R.attribute Env.env)
