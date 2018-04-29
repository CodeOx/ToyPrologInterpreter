{
open Parser

exception SyntaxError

(*type token = EOF | EOL | INT of int | T | F | PLUS | MINUS | TIMES | DIV | MOD | EXP | LPAREN | RPAREN | PERIOD | IF | COMMA | ID of string | VAR of string | EQ | GRT | LST | GRE | LTE 
			| LBRACKET | RBRACKET | BAR *)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let integer = ['0'-'9']+
let id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let var = ['A'-'Z'] ['a'-'z' 'A'-'Z' ''' '_']*
let integer = ['0'-'9'] ['0'-'9']*


rule token = parse
| white			 	{ token lexbuf }
| newline		 	{ EOL }
| "true"			{ T }
| "false"			{ F }
| "fail"			{ FAIL }
| '!'				{ CUT }
| integer as lxm	{ INT(int_of_string lxm) }
| id as lxm			{ ID(lxm) }
| var as lxm		{ VAR(lxm) }
| integer as lxm 	{ INT(int_of_string lxm) }
| '+'            	{ PLUS }
| '-'            	{ MINUS }
| '*'            	{ TIMES }
| '/'            	{ DIV }
| '%'				{ MOD }
| '^'				{ EXP }
| '('            	{ LPAREN }
| ')'            	{ RPAREN }
| '.'				{ PERIOD }
| ":-"				{ IF }
| ','				{ COMMA }
| ';'				{ COMMA }
| '='				{ EQ }
| '>'				{ GRT }
| '<'				{ LST }
| ">="				{ GRE }
| "<="				{ LTE }
| '['				{ LBRACKET }
| ']'				{ RBRACKET }
| '|'				{ BAR }
| _					{ raise SyntaxError }
| eof            	{ EOF }

{

}