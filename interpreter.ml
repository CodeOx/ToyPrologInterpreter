open PrologInterpreter

let rec string_of_term_output term = match term with
| Var a -> a
| Node(a,[]) -> a
| Node(a,b) -> (a ^ "(" ^ (string_of_termList_output b) ^ ")")

and string_of_termList_output termList = match termList with
| [] -> ""
| x::[] -> (string_of_term_output x)
| x::xs -> ((string_of_term_output x) ^ "," ^ (string_of_termList_output xs))  

let rec string_of_term term = match term with
| Var a -> ("Var(" ^ ("\"") ^ a ^ ("\"") ^ ")")
| Node(a,b) -> ("Node(" ^ ("\"") ^ a ^ ("\"") ^ ",[" ^ (string_of_termList b) ^ "])")

and string_of_termList termList = match termList with
| [] -> ""
| x::[] -> (string_of_term x)
| x::xs -> ((string_of_term x) ^ ";" ^ (string_of_termList xs))  

let string_of_atom atom = match atom with
| Cut -> "Cut"
| Fail -> "Fail"
| Atom(a,b) -> ("Atom(" ^ ("\"") ^ a ^ ("\"") ^ ",[" ^ (string_of_termList b) ^ "])")

let rec string_of_atomList atomList = match atomList with
| [] -> ""
| x::[] -> (string_of_atom x)
| x::xs -> ((string_of_atom x) ^ ";" ^ (string_of_atomList xs))
 
let rec string_of_program program = match program with
| [] -> ""
| Fact(a)::[] -> ("Fact(" ^ (string_of_atom a) ^ ")") 
| Fact(a)::xs -> ("Fact(" ^ (string_of_atom a) ^ ")" ^ "; " ^ (string_of_program xs)) 
| Rule(a,b)::[] -> ("Rule(" ^ (string_of_atom a) ^ ",[" ^ (string_of_atomList b) ^ "])")
| Rule(a,b)::xs -> ("Rule(" ^ (string_of_atom a) ^ ",[" ^ (string_of_atomList b) ^ "])"  ^ "; " ^ (string_of_program xs))

let rec string_of_unifier unifier = match unifier with 
| [] -> ""
| (Var a,b)::[] -> (a ^ " = " ^ (string_of_term_output b))
| (Var a,b)::xs -> (a ^ " = " ^ (string_of_term_output b) ^ " , " ^ (string_of_unifier xs))
| _ -> raise Error

let rec string_of_unifierList unifierList = match unifierList with
| [] -> ""
| x::[] -> string_of_unifier x
| x::xs -> ((string_of_unifier x) ^ "\n" ^ (string_of_unifierList xs))

let main () = 
    begin
        let filename =  Sys.argv.(1) in 
            let file_handle = open_in filename in
            let lexbuf = Lexing.from_channel file_handle in
                let program = Parser.program Lexer.token lexbuf in
                let choice = Sys.argv.(2) in 
                    match choice with
                    | "1" ->
                        Printf.printf "%s" ("[" ^ (string_of_program program) ^ "]");
                        print_newline();
                        flush stdout;
                    | _ ->
                        
                         while true do
                            let lexbuf = Lexing.from_channel stdin in
                            print_newline();
                            Printf.printf "%s" "| ?- ";
                            flush stdout;
                            let goal = Parser.goal Lexer.token lexbuf in
                            print_newline();
                            flush stdout;
                            try
                                let output = eval_wrapper program goal in Printf.printf "%s" (string_of_unifierList output);
                            with 
                                |True -> Printf.printf "%s" "true";
                                | False -> Printf.printf "%s" "false";
                                | Error -> Printf.printf "%s" "error";
                            print_newline();
                            flush stdout;
                        done
    end;;       

main ();;