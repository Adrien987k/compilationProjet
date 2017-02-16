%{

%}

/* Terminals */

%token ASTERISK QQUOTE QUOTE PPIPE COMMA
%token AS LOWER UPPER SUBSTRING FROM FOR
%token DOT LPAR RPAR 
%token ID
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

%nonassoc EQ NEQ LT GT LE GE
%left PLUS MINUS SLASH ASTERISK
%left OR
%left AND
%left NOT
%left IS


%type <simple_query> query
%start query

%%

