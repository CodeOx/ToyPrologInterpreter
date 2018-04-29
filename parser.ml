type token =
  | INT of (int)
  | T
  | F
  | CUT
  | FAIL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EXP
  | LPAREN
  | RPAREN
  | PERIOD
  | COMMA
  | IF
  | ID of (string)
  | VAR of (string)
  | EQ
  | GRT
  | LST
  | GRE
  | LTE
  | LBRACKET
  | RBRACKET
  | BAR
  | EOL
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
	open PrologInterpreter

	let rec unitList_to_termList unitList = match unitList with
		| [] -> []
		| (a,b)::u1 -> Node(a,b)::(unitList_to_termList u1)

	let rec unitList_to_atomList unitList = match unitList with
		| [] -> []
		| ("!",[])::u1 -> Cut::(unitList_to_atomList u1)
		| ("fail",[])::u1 -> Fail::(unitList_to_atomList u1)
		| (a,b)::u1 -> Atom(a,b)::(unitList_to_atomList u1)

# 47 "parser.ml"
let yytransl_const = [|
  258 (* T *);
  259 (* F *);
  260 (* CUT *);
  261 (* FAIL *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* MOD *);
  267 (* EXP *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* PERIOD *);
  271 (* COMMA *);
  272 (* IF *);
  275 (* EQ *);
  276 (* GRT *);
  277 (* LST *);
  278 (* GRE *);
  279 (* LTE *);
  280 (* LBRACKET *);
  281 (* RBRACKET *);
  282 (* BAR *);
  283 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  273 (* ID *);
  274 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\006\000\
\007\000\007\000\007\000\008\000\008\000\010\000\010\000\010\000\
\010\000\010\000\010\000\009\000\009\000\002\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\001\000\001\000\001\000\002\000\004\000\
\002\000\001\000\001\000\001\000\003\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\011\000\000\000\000\000\004\000\
\023\000\001\000\000\000\005\000\006\000\000\000\024\000\000\000\
\000\000\000\000\009\000\003\000\002\000\007\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\020\000\021\000\008\000\016\000\018\000\017\000\019\000"

let yydgoto = "\003\000\
\009\000\015\000\010\000\011\000\012\000\013\000\016\000\017\000\
\019\000\029\000"

let yysindex = "\009\000\
\001\000\003\255\000\000\000\000\000\000\246\254\001\000\000\000\
\000\000\000\000\001\000\000\000\000\000\011\255\000\000\004\255\
\012\255\255\254\000\000\000\000\000\000\000\000\003\255\003\255\
\000\000\009\255\008\255\019\255\020\255\021\255\000\000\255\254\
\255\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\255\024\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\011\000\238\255\
\000\000\254\255"

let yytablesize = 284
let yytable = "\028\000\
\008\000\018\000\004\000\005\000\030\000\031\000\004\000\005\000\
\020\000\001\000\002\000\014\000\021\000\037\000\039\000\026\000\
\027\000\014\000\024\000\006\000\018\000\014\000\033\000\032\000\
\022\000\025\000\023\000\012\000\012\000\038\000\040\000\034\000\
\035\000\000\000\036\000\014\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000"

let yycheck = "\018\000\
\000\000\012\001\004\001\005\001\023\000\024\000\004\001\005\001\
\007\000\001\000\002\000\001\000\011\000\032\000\033\000\017\001\
\018\001\007\000\015\001\017\001\012\001\011\000\015\001\015\001\
\014\001\014\001\016\001\013\001\014\001\032\000\033\000\013\001\
\013\001\255\255\014\001\013\001\013\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001"

let yynames_const = "\
  T\000\
  F\000\
  CUT\000\
  FAIL\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  EXP\000\
  LPAREN\000\
  RPAREN\000\
  PERIOD\000\
  COMMA\000\
  IF\000\
  EQ\000\
  GRT\000\
  LST\000\
  GRE\000\
  LTE\000\
  LBRACKET\000\
  RBRACKET\000\
  BAR\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                               ( _1 )
# 239 "parser.ml"
               : PrologInterpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                       ( _1::_2 )
# 247 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                     ( _2 )
# 254 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                 ( [] )
# 260 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 41 "parser.mly"
                  ( _1 )
# 267 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 42 "parser.mly"
                  ( _1 )
# 274 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unit) in
    Obj.repr(
# 45 "parser.mly"
                       ( match _1 with | (a,b) -> Fact(Atom(a,b)) )
# 281 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'unitList) in
    Obj.repr(
# 48 "parser.mly"
                                ( match _1 with | (a,b) -> Rule(Atom(a,b), unitList_to_atomList _3) )
# 289 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'termList) in
    Obj.repr(
# 51 "parser.mly"
                       ( (_1,_2) )
# 297 "parser.ml"
               : 'unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                  ( ("!",[]) )
# 303 "parser.ml"
               : 'unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                  ( ("fail",[]) )
# 309 "parser.ml"
               : 'unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unit) in
    Obj.repr(
# 56 "parser.mly"
                   ( [_1] )
# 316 "parser.ml"
               : 'unitList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unitList) in
    Obj.repr(
# 57 "parser.mly"
                              ( _1::_3 )
# 324 "parser.ml"
               : 'unitList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                ( [Node(_1,[])] )
# 331 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                  ( [Var(_1)] )
# 338 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unitList) in
    Obj.repr(
# 62 "parser.mly"
                            ( Node(_1,[])::(unitList_to_termList _3) )
# 346 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unitList) in
    Obj.repr(
# 63 "parser.mly"
                             ( Var(_1)::(unitList_to_termList _3) )
# 354 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unit_var_list) in
    Obj.repr(
# 64 "parser.mly"
                                ( Node(_1,[])::_3 )
# 362 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unit_var_list) in
    Obj.repr(
# 65 "parser.mly"
                                 ( Var(_1)::_3 )
# 370 "parser.ml"
               : 'unit_var_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'unitList) in
    Obj.repr(
# 67 "parser.mly"
                               ( unitList_to_termList _2 )
# 377 "parser.ml"
               : 'termList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'unit_var_list) in
    Obj.repr(
# 68 "parser.mly"
                                   ( _2 )
# 384 "parser.ml"
               : 'termList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unitList) in
    Obj.repr(
# 71 "parser.mly"
                         ( Goal (unitList_to_atomList _1) )
# 391 "parser.ml"
               : (PrologInterpreter.goal)))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry goal *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : PrologInterpreter.program)
let goal (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (PrologInterpreter.goal))
