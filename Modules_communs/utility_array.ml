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

(*Convertit tableau de float en tableau de points*)
let float_array_to_point_array float_array =
	let len = Array.length float_array in
	let point_array = Array.make (len/2) {Geometrie.x=0.;y=0.} in
	let i = ref 0 in
	while !i < (len/2) do 
		point_array.(!i) <- {Geometrie.x=float_array.(2*(!i)); y=float_array.(2*(!i)+1)};
		i := !i + 1;
	done;
	point_array;;

let float_list_to_point_array lst = 
	float_array_to_point_array (Array.of_list lst)

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

(*les sommets etant de type point, on renvoie float list avec toutes les coord*)
(* let rec linearise_sommets = fun results ->
	match sommets with 
	[] -> results |
	t::q -> util_recu q results@[t.x ; t.y];;


let obstacle_to_sommets_seuls_linearises obstacle = 
	linearise_sommets obstalcle.sommets;;

let obstacles_to_sommets_seuls_linearises obstacles = 
	let sommets_dobstacle (obs : Geometrie.obstacle) : point list = Array.to_list obs.sommets in (* prend un obstacle et renvoie ses sommets *)
	 *)
