let print ro =
  (* let Table(root, table, var_map,_) = ro in
  let high = high table in
  let low = low table in *)
  let oc = open_out "bdd2.dot" in
  let print msg = Printf.fprintf oc "%s\n" msg in
  let low_edge_type = "[color=Red, style=dashed, label=0, fontcolor=Blue]" in
  let high_edge_type = "[color=Green, label=1, fontcolor=Blue]" in
  let print_edge (x:string) (y:string) edge_type = Printf.fprintf oc "%s -> %s %s\n" x y edge_type in
  let () = 
    print "digraph G {";
    print "0 [shape=box]";
    print "1 [shape=box]"; (* Declare only when you encoutner the node *)
    (* print ("y -> 1"^high_edge_type); *)
    print_edge "y" "1" high_edge_type;
    (* print ("x -> 0"^low_edge_type); *)
    (* print ("x -> 1"^high_edge_type); *)
  in
  let () = print "}" in
  let () = close_out oc in ()

  let to_dot ro =
    let Table(root, table, var_map,_) = ro in
    let high = high table in
    let low = low table in
    
    let oc = open_out "bdd2.dot" in
    let print msg = Printf.fprintf oc "%s\n" msg in
    let low_edge_type = "[color=Red, style=dashed, label=0, fontcolor=Blue]" in
    let high_edge_type = "[color=Green, label=1, fontcolor=Blue]" in
    let print_edge (x:string) (y:string) edge_type = Printf.fprintf oc "%s -> %s %s\n" x y edge_type in
    let get_var_string u = 
      if u = 0 then "0"
      else if u = 1 then "1" 
      else Hashtbl.find var_map u
    in
    let () = print "digraph G {" in
  
    let rec construct u : unit =
      if u = 0 then print "0 [shape=box]"
      else if u = 1 print "1 [shape=box]"
      else
        let v1 = Hashtbl.find var_map u in
        let v_low = get_var_string low(u) in
        let v_high = get_var_string high(u) in
        let () = 
          print_edge v1 v_high high_edge_type;
          print_edge v1 v_low low_edge_type;
        in
        let () = construct u_low in
        let () = construct u_high in ()
    in
    let () = construct root in
    let () = print "}" in
    let () = close_out oc in ()