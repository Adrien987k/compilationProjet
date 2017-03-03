{
open Parser
exception Eof

let keyword_table = Hashtbl.create 53
let _ = 
    List.iter (fun (kwr, tok) -> Hashtbl.add keyword_table kwr tok)
              [ "ALL", ALL;
                "AND", AND;
                "AS", AS;
                "BETWEEN", BETWEEN;
                "BY", BY;
                "CROSS", CROSS;
                "DISTINCT", DISTINCT;
                "FALSE", FALSE;
                "FOR", FOR;
                "FROM", FROM;
                "FULL", FULL;
                "GROUP", GROUP;
                "HAVING", HAVING;
                "INNER", INNER;
                "IS", IS;
                "JOIN", JOIN;
                "LEFT", LEFT;
                "LOWER", LOWER;
                "NOT", NOT;
                "NULL", NULL;
                "ON", ON;
                "OR", OR;
                "OUTER", OUTER;
                "RIGHT", RIGHT;
                "SELECT", SELECT;
                "SUBSTRING", SUBSTRING;
                "TRUE", TRUE;
                "UNKNOWN", UNKNOWN;
                "UPPER", UPPER;
                "WHERE", WHERE;
                "CASE", CASE;
                "WHEN", WHEN;
                "THEN", THEN;
                "ELSE", ELSE;
                "END", END;
                "DATE", DATE;
                "CURRENT_DATE", CURRENT_DATE;
                "EXTRACT", EXTRACT;
                "YEAR", YEAR;
                "MONTH", MONTH;
                "DAY", DAY;
                "UNION", UNION;
                "EXCEPT", EXCEPT;
                "INTERSECT", INTERSECT
             ]

}


(* Déclaration du dictionnaire (regexp -> terminal/token) *)

rule anlex = parse
  | [' ' '\t' '\n' '\r']                  { anlex lexbuf }
  | "--"                                  { comlex lexbuf }
  | ';'                                   { PC }
  | '*'                                   { ASTERISK }
  | "\""                                  { QQUOTE }
  | '.'                                   { DOT }
  | '('                                   { LPAR }
  | ')'                                   { RPAR }
  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '/'                                   { SLASH }
  | "||"                                  { PPIPE }
  | ','                                   { COMMA }
  | '='                                   { EQ }
  | "<>"                                  { NEQ }
  | '<'                                   { LT }
  | '>'                                   { GT }
  | "<="                                  { LE }
  | ">="                                  { GE }
  | (['0'-'9']+ '.' (['0'-'9']+)? (('e' | 'E') ('-' | '+')? ['0'-'9']+ )? |
    '.'  ['0'-'9']+ ['0'-'9']+ ('e' | 'E' ('-' | '+')? ['0'-'9']+ )?) as lxm
                                          { FLOAT(float_of_string lxm) }
  | ['0'-'9']+ as lxm                     { INT(int_of_string lxm) }
  | ''' (([^'''] | "''")* as lxm) '''     { STRING(lxm) }
  | (['a'-'z' 'A'-'Z'] ((['a'-'z' 'A'-'Z' '0'-'9' '_']) | "é"
                                  | "à" | "ç" | "è")+ ) as lxm { try 
                                                             Hashtbl.find keyword_table lxm
                                                             with Not_found -> ID(lxm)
                                                          }
  |   ('"' (([^'"']*) as lxm) '"' )        { ID(lxm) }
  
                                          
  | eof                                   { raise Eof }
  | _ as lxm                              { 
                                             Printf.eprintf "Unknown character '%c': ignored\n" lxm; flush stderr;
                                              anlex lexbuf
                                          }

and comlex = parse
  | '\n'                     { anlex lexbuf }
  | _                        { comlex lexbuf }