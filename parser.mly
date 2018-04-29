%{
	open PrologInterpreter

	let rec unitList_to_termList unitList = match unitList with
		| [] -> []
		| (a,b)::u1 -> Node(a,b)::(unitList_to_termList u1)

	let rec unitList_to_atomList unitList = match unitList with
		| [] -> []
		| ("!",[])::u1 -> Cut::(unitList_to_atomList u1)
		| ("fail",[])::u1 -> Fail::(unitList_to_atomList u1)
		| (a,b)::u1 -> Atom(a,b)::(unitList_to_atomList u1)

%}

%token <int> INT
%token T F
%token CUT FAIL
%token PLUS MINUS TIMES DIV MOD EXP
%token LPAREN RPAREN
%token PERIOD COMMA IF
%token <string> ID
%token <string> VAR
%token EQ GRT LST GRE LTE
%token LBRACKET RBRACKET BAR
%token EOL EOF
%start program
%start goal
%type <PrologInterpreter.program> program
%type <(PrologInterpreter.goal)> goal
%%
program:
    expr	                						{ $1 }
;
expr:
	| clause expr									{ $1::$2 }
	| EOL expr										{ $2 }
	| EOF											{ [] }
;
clause:
	| fact											{ $1 }
	| rule											{ $1 }
;
fact:
	| unit PERIOD									{ match $1 with | (a,b) -> Fact(Atom(a,b)) }
;
rule:
	| unit IF unitList PERIOD						{ match $1 with | (a,b) -> Rule(Atom(a,b), unitList_to_atomList $3) }
;
unit:
	| ID termList									{ ($1,$2) }
	| CUT 											{ ("!",[]) }
	| FAIL											{ ("fail",[]) }
;
unitList:
	| unit 											{ [$1] }
	| unit COMMA unitList 							{ $1::$3 }
;
unit_var_list:
	| ID											{ [Node($1,[])] }
	| VAR 											{ [Var($1)] }
	| ID COMMA unitList 							{ Node($1,[])::(unitList_to_termList $3) }
	| VAR COMMA unitList 							{ Var($1)::(unitList_to_termList $3) }
	| ID COMMA unit_var_list 						{ Node($1,[])::$3 }
	| VAR COMMA unit_var_list 						{ Var($1)::$3 }
termList:
	| LPAREN unitList RPAREN						{ unitList_to_termList $2 }
	| LPAREN unit_var_list RPAREN					{ $2 }
;	
goal:
	unitList PERIOD									{ Goal (unitList_to_atomList $1) }