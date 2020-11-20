open Formula;;
open Program;;
module BDD = struct
		type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
		(* type robdd = bool list ;; *)
		type robdd = Table of (int * (int, int * int * int) Hashtbl.t * (int, string) Hashtbl.t);;
		        
		exception Not_implemented
		exception Malformed_Boolean_Expression
		exception Any_Sat of string

		(* let bddFromExpr bexpr order = raise Not_implemented ;; *)
		
		(* let sat_count bdd = raise Not_implemented ;; *)
		let all_sat bdd = raise Not_implemented ;;
		(* let any_sat bdd =  raise Not_implemented ;; *)
		
		let to_dot bdd = raise Not_implemented;;

		(* Helpers *)
			let hash_create x = Hashtbl.create x

		(* Table T *)
			(* let table = hash_create 123456 *)
			let table_add table u (i,l,h) = Hashtbl.add table u (i,l,h)
			let table_lookup table u = Hashtbl.find table u
			let table_member table u = Hashtbl.mem table u
			
			let var table u = let (i,_,_) = table_lookup table u in i
			let high table u = let (_,_,h) = table_lookup table u in h
			let low table u = let (_,l,_) = table_lookup table u in l

		(* Reverse Table H *)
			(* let	reverse_table = hash_create 123456 *)
			let reverse_table_insert reverse_table (i,l,h) u =  Hashtbl.add reverse_table (i,l,h) u
			let reverse_table_lookup reverse_table (i,l,h) =  Hashtbl.find reverse_table (i,l,h)
			let reverse_table_member reverse_table (i,l,h) =  Hashtbl.mem reverse_table (i,l,h)

		(* Mk function *)
			let mk (i,l,h) var_name table reverse_table var_map : int = 
				if l = h then l
				else if reverse_table_member reverse_table (i,l,h) then 
					reverse_table_lookup reverse_table (i,l,h)
				else 
					let u = (Hashtbl.length table) in
						let () = table_add table u (i,l,h) in 
						let () = reverse_table_insert reverse_table (i,l,h) u in 
						let () = Hashtbl.add var_map u var_name in (* Add mapping from u to var_name string *)
						u

		(* Evaluate Boolean Expressions*)
			let rec evaluate bexpr var_map : bool =
				match bexpr with
					| OprUnary (NOT, a) -> not (evaluate a var_map)
					| OprBinary (AND, a, b) -> (evaluate a var_map) && (evaluate b var_map)
					| OprBinary (OR, a, b) -> (evaluate a var_map) || (evaluate b var_map)
					| OprBinary (IFTHEN, a, b) -> (not (evaluate a var_map)) || (evaluate b var_map)
					| OprBinary (IFF, a, b) -> 
						let eval_a = (evaluate a var_map) in
						let eval_b = (evaluate b var_map) in 
							(eval_a && eval_b) || ((not eval_a) && (not eval_b))
					| OprTernary (IFTHENELSE, a, b, c) -> 
						if evaluate a var_map then evaluate b var_map else evaluate c var_map
					| Constant(a) -> a
					| Variable(a) -> Hashtbl.find var_map a
					| _ -> raise Malformed_Boolean_Expression

		(* Build *)
		let bddFromExpr bexpr ordering : robdd =
			let n = List.length ordering in
			let table = hash_create 123456 in
			let reverse_table = hash_create 123456 in
			let () = Hashtbl.add table 0 (n+1, -1, -1) in
			let () = Hashtbl.add table 1 (n+1, -1, -1) in
			let var_map = hash_create 123456 in (* Truth assn for variables in bexpr*)
			let node_map = hash_create 123456 in (* Mapping from node num to var_name string*)
			
			let rec build_helper i rem_order: int =
				match rem_order with
				| [] -> if evaluate bexpr var_map then 1 else 0
				| h::tail -> 
					(* var_map on exit should be the same as on entry *)
					(* Low arm *)
					let () = Hashtbl.add var_map h false in
					let v0 = build_helper (i+1) tail in
					let () = Hashtbl.remove var_map h in
					(* High arm *)
					let () = Hashtbl.add var_map h true in 
					let v1 = build_helper (i+1) tail in 
					let () = Hashtbl.remove var_map h in
					let u = mk (i, v0, v1) h table reverse_table node_map in
					u
					
			in 
			let root = build_helper 1 ordering in Table(root, table, node_map)

		let int_pow a b : int = 
			let p = float_of_int a in
			let q = float_of_int b in
			int_of_float( p**q )

		(* Sat Count *)
		let sat_count ro : int =
			let Table(root,table,_) = ro in
			let var = var table in
			let high = high table in
			let low = low table in

			let rec count u =
				if u = 0 then 0
				else if u = 1 then 1
				else 
					count(low(u))*(int_pow 2 ((var(low(u))) - (var u) - 1)) + 
					count(high(u))*(int_pow 2 ((var(high(u))) - (var u) - 1))

			in count(root)*(int_pow 2 (var(root)-1))

		let any_sat ro : sat_assignment =
			let Table(root, table, var_map) = ro in
			let high = high table in
			let low = low table in

			let rec helper u : sat_assignment = 
				if u = 0 then raise (Any_Sat "Boolean expression is a contradiction. No sat_assignment possible.")
				else if u = 1 then []
				else if low(u) = 0 then 
					let str = Hashtbl.find var_map u in str::(helper(high(u)))
				else 
					helper(low(u))

			in helper(root)

end;;
(* let a = (Program.Constant true);;
a;; *)

(* let build_helper i =
	let n = List.length bexpr in
		if i > n then 
			if evaluate bexpr var_map then 1 else 0
		else
			let h::t = ordering in
				let () = Hashtbl.add var_map h 0
				let v0 = build_helper (i+1)
				let () = Hashtbl.remove var_map h

				let () = Hashtbl.add var_map h 1
				let v1 = build_helper (i+1)
				let () = Hashtbl.remove var_map h
				mk (i, v0, v1) *)


(* 
let rec build_helper i rem_order bexpr var_map node_map table reverse_table: int =
	match rem_order with
		| [] -> if evaluate bexpr var_map then 1 else 0
		| h::tail -> 
			(* var_map on exit should be the same as on entry *)
				(* Low arm *)
				let () = Hashtbl.add var_map h false in
				let v0 = build_helper (i+1) tail bexpr var_map node_map table reverse_table in
				let () = Hashtbl.remove var_map h in
				(* High arm *)
				let () = Hashtbl.add var_map h true in 
				let v1 = build_helper (i+1) tail bexpr var_map node_map table reverse_table in 
				let () = Hashtbl.remove var_map h in
				let u = mk table reverse_table (i, v0, v1) in
				(* Add mapping from u to var_name string *)
				let () = Hashtbl.add node_map u h in
				u *)

(* let rec count u =
			if u = 0 then 0
			else if u = 1 then 1
			else 
				count(low(u))*(int_pow 2 ((var(low(u))) - (var u) - 1)) + 
				count(high(u))*(int_pow 2 ((var(high(u))) - (var u) - 1)) *)