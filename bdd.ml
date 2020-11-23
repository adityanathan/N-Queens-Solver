open Formula;;
open Program;;

(* open Format;;
open Graph;; *)
(* #require "ocamlgraph";; *)
(* open Ocamlgraph;; *)

(* module Node = struct
	type t = string
	let compare = Stdlib.compare
	let hash = Hashtbl.hash
	let equal = (=)
end

module Edge = struct
	type t = string
	let compare = Stdlib.compare
	let equal = (=)
	let default = ""
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

module Dot = Graph.Graphviz.Dot(struct
	include G (* use the graph module from above *)
	let edge_attributes (a, e, b) = [`Label e; `Color 4711]
	let default_edge_attributes _ = []
	let get_subgraph _ = None
	let vertex_attributes _ = [`Shape `Box]
	let vertex_name v = string_of_int v
	let default_vertex_attributes _ = []
 let graph_attributes _ = []
end) *)


module BDD = struct
		type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
		(* type robdd = bool list ;; *)
		type robdd = Table of (int * (int, int * int * int) Hashtbl.t * (int, string) Hashtbl.t * string list);;
		        
		exception Not_implemented
		exception Malformed_Boolean_Expression
		exception Any_Sat of string

		(* let bddFromExpr bexpr order = raise Not_implemented ;; *)
		
		(* let sat_count bdd = raise Not_implemented ;; *)
		(* let all_sat bdd = raise Not_implemented ;; *)
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
				(* else if u = 1 then [[]] *)
				else if u = 1 then combinations visited_map ordering
				else 
					let str = Hashtbl.find var_map u in
					let () = Hashtbl.add visited_map str 1 in
					let low_lst = helper(low(u)) in
					let high_lst = helper(high(u)) in
					let () = Hashtbl.remove visited_map str in
					(low_lst)@(List.map (List.cons str) (high_lst))
				in remove_duplicates(helper root)

		(* let to_dot robdd =
			let Table(root, table, var_map) = ro in
			let high = high table in
			let low = low table in
			let g = G.empty in

			let rec construct u : unit =
				if u = 0 then ()
				else if u = 1 then ()
				else
					let u_low = low(u) in
					let u_high = high(u) in
					let v1 = G.V.create(Hashtbl.find var_map u) in
					let v_low = G.V.create(Hashtbl.find var_map u_low) in
					let v_high = G.V.create(Hashtbl.find var_map u_high) in
					let g = G.add_vertex g v1 in
					let g = G.add_vertex g v_low in
					let g = G.add_vertex g v_high in
					let e_low = E.create v1 "1" v_high in
					let e_high = E.create v1 "0" v_low in
					let g = G.add_edge_e g e_low in
					let g = G.add_edge_e g e_high in
					let () = construct u_low in
					let () = construct u_high in ()
			in
			let file = open_out_bin "bdd.dot" in
			let () = Dot.output_graph file g in () *)

end;;

(* let all_sat ro : sat_assignment list =
	let Table(root, table, var_map) = ro in
	let high = high table in
	let low = low table in

	let rec helper u : sat_assignment list =
		if u = 0 then []
		else if u = 1 then [[]]
		else 
			let low_lst = helper(low(u)) in
			let high_lst = helper(high(u)) in
			let str = Hashtbl.find var_map u in
			(low_lst)@(List.map (List.cons str) (high_lst))
		in helper root *)