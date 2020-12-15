open Geometrie

let copie_tab = fun tab ->
	let n = Array.length tab in
	let tab2 = Array.make n 0. in
	for i = 0 to (n-1) do
		tab2.(i) <- tab.(i);
	done;
	tab2;;


(*transforme un flaot array en point list*)
let array_to_point tab = 
	let len = Array.length tab in 
	let lst = ref [] in
	let i = ref (len-2) in 
	while !i >= 0 do 
		lst := {x = tab.(!i); y = tab.(!i+1)} :: !lst;
		i := !i-2;
	done;
	!lst;;

(* returns an array where points are arrays of length = 2 *)
let point_array_to_floataa points_list = 
    let rec point_array_to_floataa_rec = fun lst tab ->
		match lst with 
		[] -> tab|
		t :: q -> point_array_to_floataa_rec q (Array.append tab [| [| t.x ; t.y|] |]) in
	point_array_to_floataa_rec points_list [||];;

(* returns a list where points are lists of length = 2 *)
let point_array_to_floatll points_array = 
	let points_list = Array.to_list points_array in 
	let rec point_array_to_floatll_rec = fun lst tab ->
			match lst with 
			[] -> tab|
			t :: q -> point_array_to_floatll_rec q (List.append tab [ [ t.x ; t.y] ]) in
		point_array_to_floatll_rec points_list [];;