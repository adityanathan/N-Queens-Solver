open Formula;;
open Bdd;;

open Program;;

let cstr i j = Variable("c"^(string_of_int i)^(string_of_int j))

let cstr2 i j = "c"^(string_of_int i)^(string_of_int j)

(* start = 0 , n should be actual n *) 
let rec generate_row iter n i j : bool_expr =
	if (iter < n) && (iter >= 0) then 
		if (iter <> j) then
			OprBinary(AND, OprUnary(NOT, cstr i iter), generate_row (iter+1) n i j)
		else
			generate_row (iter+1) n i j
	else
		Constant(true)

let rec generate_col iter n i j : bool_expr =
	if (iter < n) && (iter >= 0) then 
		if (iter <> i) then
			OprBinary(AND, OprUnary(NOT, cstr iter j), generate_col (iter+1) n i j)
		else
			generate_col (iter+1) n i j
	else
		Constant(true)

let rec generate_diag iter n i j : bool_expr =
	if (iter < n) && (iter >= 0) then 
		if (iter <> i) && (j-i+iter >= 0) && (j-i+iter < n) then
			OprBinary(AND, OprUnary(NOT, cstr iter (j-i+iter)), generate_diag (iter+1) n i j)
		else
			generate_diag (iter+1) n i j
	else
		Constant(true)

let rec generate_rev_diag iter n i j : bool_expr =
	if (iter < n) && (iter >= 0) then 
		if (iter <> i) && (j+i-iter >= 0) && (j+i-iter < n) then
			OprBinary(AND, OprUnary(NOT, cstr iter (j+i-iter)), generate_rev_diag (iter+1) n i j)
		else
			generate_rev_diag (iter+1) n i j
	else
		Constant(true)

let rec generate_or iter n i : bool_expr =
	if (iter < n) && (iter >= 0) then 
		OprBinary(OR, cstr i iter, generate_or (iter+1) n i)
	else
		Constant(false)

let rec all_generate_or row n: bool_expr =
	if (row < n) && (row >= 0) then 
		OprBinary(AND, generate_or 0 n row, all_generate_or (row+1) n)
	else
		Constant(true)

let rec and_all (lst: bool_expr list) : bool_expr =
	match lst with
	| hd::[] -> hd
	| hd::tl -> OprBinary(AND, hd, and_all tl)

let generate_rcd i j n : bool_expr =
	let a = generate_row 0 n i j in
	let b = generate_col 0 n i j in
	let c = generate_diag 0 n i j in
	let d = generate_rev_diag 0 n i j in
	let temp_fn x = OprBinary(IFTHEN, cstr i j, x) in
	and_all [temp_fn a; temp_fn b; temp_fn c; temp_fn d]

let rec loop2 i j n : bool_expr =
	if (j < n) && (j >= 0) then 
		OprBinary(AND, generate_rcd i j n, loop2 i (j+1) n)
	else
		Constant(true)

let rec loop1 i n : bool_expr =
	if (i < n) && (i >= 0) then 
		OprBinary(AND, loop2 i 0 n, loop1 (i+1) n)
	else
		Constant(true)

let generate_formula n : bool_expr =
	let first = loop1 0 n in
	let second = all_generate_or 0 n in
	OprBinary(AND, first, second)


let rec loop2_ordering i j n : string list =
	if (j < n) && (j >= 0) then 
		(cstr2 i j)::(loop2_ordering i (j+1) n)
	else []

let rec loop1_ordering i n : string list =
	if (i < n) && (i >= 0) then 
		List.append (loop2_ordering i 0 n) (loop1_ordering (i+1) n)
	else
		[]

let generate_ordering n = loop1_ordering 0 n

(* let n_queen board_size = raise BDD.Not_implemented ;; *)
let n_queen board_size =
	let formula = generate_formula board_size in
	let ordering = generate_ordering board_size in
	let new_robdd = BDD.bddFromExpr formula ordering in
	BDD.any_sat new_robdd

(* 	The type of n_queen is int -> string list, where int is the 
	size of board. The solution is represented as list of strings, 
	where each string denotes the position of a queen in the solution. 
	The position is a string 'c' (lower case c without quotes) 
	appended with two single digit integers i and j, where i and j 
	are row and column numbers respectively, starting from 0. 
	For example, the string for cell in row 0 and column 4 should be c04. 
*)


(*	The type of  knight is int -> int -> int -> string list, where
	first int is board size, second and third ints represent the
	row and column number (starting from 0) respectively of 
	initial position of the knight on the board
	The output to this function should be a sequence of strings.
	The first element in the sequence should be cell name 
	corresponding to the initial position of the knight. 
	Each subsequent string in the sequence should represent the 
	next cell visited by the knight. 
*)
let knight board_size init_row init_col = raise BDD.Not_implemented ;;	

