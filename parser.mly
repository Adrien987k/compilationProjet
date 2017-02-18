%{
	open Ast
%}

/* Terminals */

%token ASTERISK QQUOTE QUOTE PPIPE COMMA
%token AS LOWER UPPER SUBSTRING FROM FOR
%token DOT LPAR RPAR 
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token PLUS MINUS SLASH
%token EQ NEQ LT GT LE GE
%token ALL AND AS BETWEEN BY CROSS DISTINCT FALSE FOR
		FROM FULL GROUP HAVING INNER IS JOIN LEFT LOWER
		NOT NULL ON OR OUTER RIGHT SELECT SUBSTRING TRUE UNKNOWN
		UPPER WHERE 

/* Priorities and associativity */

/* ------------------ TO DO ------------------  */

%nonassoc EQ NEQ LT GT LE GE
%left PLUS MINUS SLASH ASTERISK
%left OR
%left AND
%left NOT
%left IS

%type <simple_query> ansyn
%start ansyn

%%

ansyn:
	| query { $1 }
;

query:
	| SELECT projection FROM source WHERE condition				{ cst_squerySelectFromWhere $2 $4 $6 }
	| SELECT ALL projection FROM source WHERE condition			{ cst_squerySelectAllFromWhere $3 $5 $7 }
	| SELECT DISTINCT projection FROM source WHERE condition	{ cst_squerySelectDistinctFromWhere $3 $5 $7 }
;

column:
	| expression			{ cst_columnExpr $1 }
	| expression STRING		{ cst_columnExprId $1 $2 }

columnExtends:
	| column 								{ cst_columnExtendsSingle $1 }
	| columnExtends COMMA columnExtends 	{ cst_columnExtendsMany $1 $3 }
;

projection:
	| ASTERISK			{ cst_projAsterisk }
	| columnExtends		{ cst_projColumns $1 }
;


source:
	| ID 										{ cst_sourId $1 }
	| LPAR query RPAR							{ cst_sourSQuery $2 }
	| source COMMA source						{ cst_sourComma $1 $3 }
	| source CROSS JOIN source					{ cst_sourCrossJoin $1 $4 }
	| source JOIN source ON condition 			{ cst_sourJoinOn $1 (cst_join) $3 $5 }
	| source INNER JOIN source ON condition		{ cst_sourJoinOn $1 (cst_innerjoin) $4 $6}
	| source RIGHT source ON condition			{ cst_sourJoinOn $1 (cst_right) $3 $5 }
	| source LEFT source ON condition			{ cst_sourJoinOn $1 (cst_left) $3 $5 }
	| source FULL source ON condition			{ cst_sourJoinOn $1 (cst_full) $3 $5 }
	| source OUTER RIGHT source ON condition	{ cst_sourJoinOn $1 (cst_outerright) $4 $6}
	| source OUTER LEFT source ON condition		{ cst_sourJoinOn $1 (cst_outerleft) $4 $6}
	| source OUTER FULL source ON condition		{ cst_sourJoinOn $1 (cst_outerfull) $4 $6}
;

condition:
	| predicate 						{ cst_condPred $1 }
	| NOT condition						{ cst_condNotCond $2 }
	| condition AND condition			{ cst_condAnd $1 $3 }
	| condition OR condition			{ cst_condOr $1 $3 }
	| condition IS TRUE					{ cst_condIsTrue $1 }
	| condition IS NOT TRUE				{ cst_condIsNotTrue $1 }
	| condition IS FALSE				{ cst_condIsFalse $1 }
	| condition IS NOT FALSE			{ cst_condIsNotFalse $1 }
	| condition IS UNKNOWN				{ cst_condIsUnknown $1 }
	| condition IS NOT UNKNOWN			{ cst_condIsNotUnknown $1 }
;

predicate:
	| LPAR condition RPAR								{ cst_predCond $2 }
	| expression EQ expression							{ cst_predEq $1 $3 }
	| expression NEQ expression							{ cst_predNeq $1 $3 }
	| expression LT expression							{ cst_predLt $1 $3 }
	| expression LE expression							{ cst_predLe $1 $3 }
	| expression GT expression							{ cst_predGt $1 $3 }
	| expression GE expression							{ cst_predGe $1 $3 }
	| expression BETWEEN expression AND expression		{ cst_predBetween $1 $3 $5 }
	| expression NOT BETWEEN expression AND expression	{ cst_predNotBetween $1 $4 $6}
	| expression IS NULL								{ cst_predNull $1 }
	| expression IS NOT NULL 							{ cst_predNotNull $1 }
;

expression:
	| ID DOT ID 													{ cst_exprAttribute $1 $3 }
	| LPAR expression RPAR											{ cst_exprPar $2 }
	| INT 															{ cst_exprInt $1 }
	| FLOAT 														{ cst_exprFloat $1 }
	| expression PLUS expression 									{ cst_exprPlus $1 $3 }
	| expression MINUS expression 									{ cst_exprMinus $1 $3 }
	| expression ASTERISK expression 								{ cst_exprAsterisk $1 $3 }
	| expression SLASH expression 									{ cst_exprSlash $1 $3 }
	| MINUS expression 												{ cst_exprUMinus $2 }
	| STRING 														{ cst_exprString $1 }
	| expression PPIPE expression 									{ cst_exprPipe $1 $3 }
	| LOWER LPAR expression RPAR 									{ cst_exprLower $3 }
	| UPPER LPAR expression RPAR 									{ cst_exprUpper $3 }
	| SUBSTRING LPAR expression FROM expression FOR expression RPAR { cst_exprSubString $3 $5 $7}
;