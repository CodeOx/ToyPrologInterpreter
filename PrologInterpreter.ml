exception Error
exception NOT_UNIFIABLE
exception FAIL

type term = Var of string | Node of string*(term list)	(* a constant is a zero-ary function symbol *)

type atom = Atom of string * (term list)

type clause = Fact of atom | Rule of atom*(atom list)

type program = clause list

type goal = Goal of atom list


let id x = x  

let sum a b = a + b

let rec max l a = match l with
	| [] -> a
	| x::xs -> if (x > a) then (max xs x) else (max xs a)

(* max function gives the maximum value in list l, a is accumulator to make the function tail recursive *)

let rec listAnd b l= match l with 
	| [] -> b
	| x::xs -> listAnd (b && x) xs

(* listAnd takes a list of booleans and performs "and" operation on all these booleans as well as a boolean b (used to make the function tail recursive) *)

let rec listContains l a = match l with
	| [] -> false
	| x::xs -> if (x = a) then true else listContains xs a

(* listContains checks whether a list l contains the element a *)

let rec union l1 l2 = match l1 with 
	| [] -> l2 
	| x::xs -> if (listContains l2 x) then (union xs l2) else (union xs (x::l2))

(* Union takes two lists l1 and l2, and returns a list which has all the elements of l2 and those elements of l1 which are not in l2 *)

let rec getTerm l a = match l with
	| [] -> a
	| (Var x,b)::xs -> if ((Var x) = a) then b else getTerm xs a
	| _ -> raise Error

(* getTerm takes a substitution and a variable and gives the term that the variable maps to in the substitution *)					 

let rec vars vlist term = match term with
	| Var a -> (Var a)::vlist
	| Node (a,b) -> let c = (List.map (vars []) b) in (
		List.fold_left union [] c
	)

(* vars gives the set of variables in a well formed term *)

(* substitutions are represented as list of pairs of variables and terms, representing the mapping of the variable to the corresponding term *)
(* composition of substitutions are represented as list of substitutions with first element of list (first substitution) applied first *)

let rec subst s term = match term with
	| Var a -> getTerm s (Var a)
	| Node (a,b) -> Node (a, (List.map (subst s) b))

(* subst takes a substitution s and a term t and applies the Unique Homomorphic Extension of s to t *)

let rec pairListContains l x = match l with
	| [] -> false
	| (a,b)::c -> if (x = a) then true else (pairListContains c x)

(* pairListContains checks if a list of pairs contains x as the first element of any pair *)

let rec pairUnion l1 l2 = match l2 with
	| [] -> l1
	| (a, b)::c -> if (pairListContains l1 a) then (pairUnion l1 c) else (pairUnion ((a,b)::l1) c)

(* Union takes two lists of pairs l1 and l2, and returns a list of pairs which has all the elements of l1 and those elements of l2 which are not in l1, equality 
	 being checked by the first element of the pairs *)

let rec substSigma sigma1 sigma2 = match sigma1 with
	| [] ->[]
	| (Var a, b)::xs -> (Var a, subst sigma2 b)::(substSigma xs sigma2)
	| _ -> raise Error

(* substSigma takes two substitution s1 and s2 and applies UHE of s2 to each term in s1 *)

let composePair sigma1 sigma2 = let sigma3 = (substSigma sigma1 sigma2) in pairUnion sigma3 sigma2

(* composePair takes two substitutions and returns the composite substitution of the two *)

let rec compose subsList = match subsList with
	| [] -> []
	| x::[] -> x
	| x1::x2::xs -> compose ((composePair x1 x2)::xs)

(* compose function takes a composition of substitutions and gives a final substitution, 
	 eg : sigma1 : x1 -> t1, x2 -> x3; sigma2 : x4 -> t4, x3 -> t3; where x(i) repesents variables and t(i) represents terms;
	 then sigma1 = [(x1,t1); (x2,x3)]; sigma2 = [(x4,t4);(x3,t3)]; composition of sigma1,sigma2 = [[(x1,t1); (x2,x3)];[(x4,t4);(x3,t3)]] 
	 compose (composition of sigma1,sigma2) = [(x1,t1);(x2,t3);(x3,t3);(x4,t4)] {Not necessarily in this order}
*)

let rec mgu subsList t1 t2 = match (t1,t2) with
	| (Var x, Var y) -> if x = y then [] else [(Var x, Var y)]
	| (Var x, Node(a,b)) -> let v = (vars [] t2) in
		if (listContains v t1) then raise NOT_UNIFIABLE else [(t1,t2)]
	| (Node(a,b), Var x) -> let v = (vars [] t1) in
		if (listContains v t2) then raise NOT_UNIFIABLE else [(t2,t1)]
	| (Node(a,b),Node(c,d)) -> if a = c && (List.length b) = (List.length d) then 
			(match (b,d) with
				| ([],[]) -> compose subsList
				| (e::es,f::fs) -> let sigma1 = mgu [] e f in 
					let subst1 = subst (compose (subsList@[sigma1])) in
						mgu (subsList@[sigma1]) (Node(a,(List.map subst1 es))) (Node(c,(List.map subst1 fs))) 
				| (_,_) -> raise NOT_UNIFIABLE) 
		else
		 raise NOT_UNIFIABLE

let mgu_atoms a1 a2 = match (a1,a2) with
	| (Atom (x1,y1),Atom (x2,y2)) -> mgu [] (Node (x1,y1)) (Node (x2,y2))

let rec subst_atom s atom = match atom with
	| Atom (a,b) -> Atom (a, (List.map (subst s) b))

(* subst_atom takes a substitution s and an atom and applies the Unique Homomorphic Extension of s to atom *)

let rec eval unifierList currentUnif originalProg prog goal = match goal with
	| Goal [] -> currentUnif::unifierList
	| Goal (x::xs) -> let substituted_atomic_goal = subst_atom currentUnif x in
		(match prog with
		| [] -> unifierList
		| (Fact f)::p1 -> (let substituted_fact = subst_atom currentUnif f in
			try (eval [] (mgu_atoms substituted_atomic_goal substituted_fact) originalProg originalProg (Goal xs))@(eval [] currentUnif originalProg p1 goal)@unifierList
			with NOT_UNIFIABLE -> (eval [] currentUnif originalProg p1 goal ))@unifierList
		| (Rule (r,l))::p1 -> (let substituted_fact = subst_atom currentUnif r in
			try (eval [] (mgu_atoms substituted_atomic_goal substituted_fact) originalProg originalProg (Goal (l@xs)))@(eval [] currentUnif originalProg p1 goal)@unifierList
			with NOT_UNIFIABLE -> (eval [] currentUnif originalProg p1 goal)@unifierList ))

(*  unifierList contains list of all solutions found so far
	currentUnif is the current Solution used in recursion
	originalProg is the program to begin with
	prog is the current program in the recursion
	originalGoal is the goal to begin with
	goal is all the remaining goals in recursion
 *)

(* eval takes a program and a goal and gives the first solution to the goal *)

let eval_wrapper program goal = eval [] [] program program goal

(* eval_wrapper is a wrapper for the eval function *)
;;

let p = [Fact(Atom("edge",[Node("a",[]);Node("b",[])])); Fact(Atom("edge",[Node("a",[]);Node("c",[])])); Fact(Atom("path",[Var ("x"); Var("x")])); Rule(Atom("path",[Var ("x"); Var("y")]),[Atom("edge",[Var("x"); Var("y")])])];;
let g1 = Goal [Atom("edge",[Node("a",[]); Var("x")])];;
let g2 = Goal [Atom("path",[Node("b",[]); Var("x")])];;
let g3 = Goal [Atom("path",[Node("a",[]); Node("b",[])])];;
let g4 = Goal [Atom("edge",[Var("y"); Var("x")])];;
let g5 = Goal [Atom("path",[Var("x");Node("b",[])])];;