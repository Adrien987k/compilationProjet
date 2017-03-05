%{
	open Sqldate
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
		UPPER WHERE CASE WHEN THEN ELSE END 
		EXTRACT CURRENT_DATE DATE YEAR MONTH DAY
		UNION EXCEPT INTERSECT
		NATURAL
%token PC
/* Priorities and associativity */

/* ------------------ TO DO ------------------  */

%left UNION UNIONALL EXCEPT EXCEPTALL
%left INTERSECT INTERSECTALL
%nonassoc SQUERY
%nonassoc QUERY
%nonassoc EMPTY
%nonassoc CROSSJOIN SRCCOMMA 
%nonassoc CROSS FULL INNER JOIN LEFT OUTER RIGHT ASTERISK
%nonassoc EQ NEQ LT GT LE GE
%left PPIPE COMMA
%left PLUS MINUS SLASH TIMES
%left OR
%left AND
%left NOT
%left IS
%nonassoc PC

%type <query> ansyn
%start ansyn

%%

ansyn:
	| PC ansyn    			 { $2 } 
	| query PC  		     { $1 }
;

query:
	| simple_query          %prec SQUERY        { cst_querySimple $1 }
	| query UNION query 						{ cst_queryUnion $1 $3 }
	| query UNION ALL query      %prec UNIONALL					{ cst_queryUnionAll $1 $4 }
	| query EXCEPT query 						{ cst_queryExcept $1 $3 }
	| query EXCEPT ALL query     %prec EXCEPTALL 					{ cst_queryExceptAll $1 $4 }
	| query INTERSECT query 					{ cst_queryIntersect $1 $3 }
	| query INTERSECT ALL query  %prec INTERSECTALL                { cst_queryIntersectAll $1 $4 }   
;

simple_query:
	| SELECT projection FROM source								{ cst_squerySelectFrom $2 $4 }
	| SELECT ALL projection FROM source							{ cst_squerySelectAllFrom $3 $5 }
	| SELECT DISTINCT projection FROM source					{ cst_squerySelectDistinctFrom $3 $5 }
	| SELECT projection FROM source WHERE condition				{ cst_squerySelectFromWhere $2 $4 $6 }
	| SELECT ALL projection FROM source WHERE condition			{ cst_squerySelectAllFromWhere $3 $5 $7 }
	| SELECT DISTINCT projection FROM source WHERE condition	{ cst_squerySelectDistinctFromWhere $3 $5 $7 }
;

column:
	| expression			{ cst_columnExpr $1 }
	| expression AS ID		{ cst_columnExprId $1 $3 }

columnExtends:
	| column 								{ cst_columnExtendsSingle $1 }
	| column COMMA columnExtends 	{ cst_columnExtendsMany $1 $3 }
;

projection:
	| ASTERISK			{ cst_projAsterisk }
	| columnExtends		{ cst_projColumns $1 }
;

source:
	| ID 										{ cst_sourId $1 }
	| LPAR query RPAR							{ cst_sourQuery $2 }
	| source COMMA source				%prec SRCCOMMA		{ cst_sourComma $1 $3 }
	| source CROSS JOIN source			%prec CROSSJOIN		{ cst_sourCrossJoin $1 $4 }
	| source natural JOIN source ON condition 			{ cst_sourJoinOn $1 $2 (cst_join) $4 $6 }
	| source natural INNER JOIN source ON condition		 { cst_sourJoinOn $1 $2 (cst_innerjoin) $5 $7 }
	| source natural RIGHT JOIN source ON condition			{ cst_sourJoinOn $1 $2 (cst_right) $5 $7 }
	| source natural LEFT JOIN source ON condition			{ cst_sourJoinOn $1 $2 (cst_left) $5 $7 }
	| source natural FULL JOIN source ON condition		{ cst_sourJoinOn $1 $2 (cst_full) $5 $7 }
	| source natural RIGHT OUTER JOIN source ON condition	{ cst_sourJoinOn $1 $2 (cst_outerright) $6 $8 }
	| source natural LEFT OUTER JOIN source ON condition 		{ cst_sourJoinOn $1 $2 (cst_outerleft) $6 $8 }
	| source natural FULL OUTER JOIN source ON condition		{ cst_sourJoinOn $1 $2 (cst_outerfull) $6 $8 }
;

natural:
	|        			%prec EMPTY                                       { cst_noNatural }
	| NATURAL                                       { cst_natural }

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
	| ID 															{ cst_exprId $1 }
	| ID DOT ID 													{ cst_exprAttribute $1 $3 }
	| LPAR expression RPAR											{ cst_exprPar $2 }
	| INT 															{ cst_exprInt $1 }
	| FLOAT 														{ cst_exprFloat $1 }
	| expression PLUS expression 									{ cst_exprPlus $1 $3 }
	| expression MINUS expression 									{ cst_exprMinus $1 $3 }
	| expression ASTERISK expression %prec TIMES 					{ cst_exprAsterisk $1 $3 }
	| expression SLASH expression 									{ cst_exprSlash $1 $3 }
	| MINUS expression 												{ cst_exprUMinus $2 }
	| STRING 														{ cst_exprString $1 }
	| expression PPIPE expression 									{ cst_exprPPipe $1 $3 }
	| LOWER LPAR expression RPAR 									{ cst_exprLower $3 }
	| UPPER LPAR expression RPAR 									{ cst_exprUpper $3 }
	| SUBSTRING LPAR expression FROM expression FOR expression RPAR { cst_exprSubString $3 $5 $7}
	| CASE expression when_expr_then END                            { cst_exprCaseExpr $2 $3 }
	| CASE expression when_expr_then ELSE expression END 			{ cst_exprCaseExprElse $2 $3 $5 }
	| CASE when_cond_then END                                       { cst_exprCaseCond $2 }
	| CASE when_cond_then ELSE expression END                       { cst_exprCaseCondElse $2 $4 }
	| date                                                          { cst_exprDate $1 }
	| EXTRACT LPAR date_field FROM date RPAR						{ cst_exprExtract $3 $5 }
;

date:
	| CURRENT_DATE													{ cst_dateCurrent }
	| DATE STRING                                                   { cst_dateDate (date_of_string $2) }

date_field:
	| YEAR															{ cst_dateFieldYear }
	| MONTH															{ cst_dateFieldMonth }
	| DAY															{ cst_dateFieldDay }

when_expr_then:
	| WHEN expression THEN expression						{ cst_whenExprThen $2 $4 }
	| WHEN expression THEN expression when_expr_then        { cst_whenExprThenExtends $2 $4 $5 }
;

when_cond_then:
	| WHEN condition THEN expression                        { cst_whenCondThen $2 $4 }
	| WHEN condition THEN expression when_cond_then         { cst_whenCondThenExtends $2 $4 $5 }
;