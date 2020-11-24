open Formula;;
open Program;;

module BDD = struct
		type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
		(* type robdd = bool list ;; *)
		type robdd = Table of (int * (int, int * int * int) Hashtbl.t * (int, string) Hashtbl.t * string list);;
		        
		exception Not_implemented
		exception Malformed_Boolean_Expression
		exception Any_Sat of string

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
			let root = build_helper 1 ordering in Table(root, table, node_map, ordering)

		let int_pow a b : int = 
			let p = float_of_int a in
			let q = float_of_int b in
			int_of_float( p**q )

		(* Sat Count *)
		let sat_count ro : int =
			let Table(root,table,_, _) = ro in
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
			let Table(root, table, var_map, _) = ro in
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

		let rec generate_combinations lst =
			match lst with
			| [] -> [[]]
			| hd::tl -> 
				let temp = (generate_combinations tl) in 
					List.append temp (List.map (List.cons hd) temp)

		let combinations visited_map ordering =
			generate_combinations (List.filter (fun x -> not (Hashtbl.mem visited_map x)) ordering)

		let remove_duplicates lst =
			let map = Hashtbl.create 123456 in
			let rec helper l =
				match l with
				| [] -> []
				| hd::tl ->
						if Hashtbl.mem map hd then helper tl
						else
							let () = Hashtbl.add map hd 1 in
							let new_l = helper tl in
							let () = Hashtbl.remove map hd in
							hd::new_l
			in helper lst 

		let all_sat ro : sat_assignment list =
			let Table(root, table, var_map, ordering) = ro in
			let high = high table in
			let low = low table in
			let visited_map = hash_create 123456 in

			let rec helper u : sat_assignment list =
				if u = 0 then []
				else if u = 1 then combinations visited_map ordering
				else 
					let str = Hashtbl.find var_map u in
					let () = Hashtbl.add visited_map str 1 in
					let low_lst = helper(low(u)) in
					let high_lst = helper(high(u)) in
					let () = Hashtbl.remove visited_map str in
					(low_lst)@(List.map (List.cons str) (high_lst))
				in remove_duplicates(helper root)

			let to_dot ro =
				let Table(root, table, var_map,_) = ro in
				let high = high table in
				let low = low table in
				
				let oc = open_out "bdd2.dot" in
				let print msg = Printf.fprintf oc "%s\n" msg in
				let low_edge_type = "[color=Red, style=dashed, label=0, fontcolor=Blue]" in
				let high_edge_type = "[color=Green, label=1, fontcolor=Blue]" in
				let print_vertex (ux, x) =
					Printf.fprintf oc "%s_%i [label=\"%s\"]\n" x ux x in
				let print_edge (ux, x) (uy, y) edge_type =
					if uy = 0 || uy = 1 then
						Printf.fprintf oc "%s_%i -> %s %s\n" x ux y edge_type
					else 
						Printf.fprintf oc "%s_%i -> %s_%i %s\n" x ux y uy edge_type
				in
				let get_var_string u = 
					if u = 0 then "0"
					else if u = 1 then "1" 
					else Hashtbl.find var_map u
				in
				let () = print "digraph G {" in
				let print_hashmap = Hashtbl.create 123456 in
				let member x = Hashtbl.mem print_hashmap x in
				let add x = Hashtbl.add print_hashmap x 1 in
			
				let rec construct u : unit =
					if u = 0 then 
						if member "0" then () else let () = add "0" in print "0 [shape=box]"
					else if u = 1 then
						if member "1" then () else let () = add "1" in print "1 [shape=box]"
					else
						let v1 = Hashtbl.find var_map u in
						let u_low = low u in
						let v_low = get_var_string u_low in
						let u_high = high u in
						let v_high = get_var_string u_high in
						let () =
							let str = v1^"_"^(string_of_int u) in
							if member str then () else let () = add str in print_vertex (u, v1)
						in
						let () = 
							print_edge (u, v1) (u_high, v_high) high_edge_type;
							print_edge (u, v1) (u_low, v_low) low_edge_type;
						in
						let () = construct (low u) in
						let () = construct (high u) in 
						()
				in
				let () = construct root in
				let () = print "}" in
				let () = close_out oc in
				()
end;;