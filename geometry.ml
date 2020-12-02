module type DOMAIN = sig
	type t
	val distance : t -> t -> float
	val equation_droite : t -> t -> float * float
	val intersection_droites : t -> t -> t
	val eliminatoire : t -> t -> bool
	val equation_obstacle : t -> (float * float) list
	val croise_obstacle : t -> t -> bool
	val trajectoire_ok : t list -> t list -> bool
end;;

module Geometrie = struct
	type point = {x : float; y : float}

	type element = 
		Point of point 
		| Polygone of element list 
		| Segment of point * point

		(* Fonction qui donne les points composant un élément *)
	let give_pt = fun elt ->
		match elt with
			| Point(p) -> p
			| _ -> failwith "pas un point"
			
	let rec give_poly = fun elt ->
		match elt with
			| Polygone(t::q) -> (give_pt t)::(give_poly Polygone(q))
			| _ -> failwith "pas un polygone"
			
	let give_seg = fun elt ->
		match elt with
			| Segment(a,b) -> a,b
			|_ -> failwith "pas un segment"		

	(* Donne la différence acceptable entre deux flottants *)
	let epsilon = 10.**(-5.)

	(* Fonction qui donne la distance entre deux points *)
	let distance = fun a b -> sqrt (((a.x -. b.x)**2.) +. ((a.y -. b.y)**2.))

	(* Fonction qui donne les paramètres d'une droite connaissant deux de ses points *)
	let equation_droite = fun a b -> let m = (b.y -. a.y)/.(b.x -. a.x) in m, (a.y -. m *. a.x)

	(* Fonction qui donne les paramètres des droites formant le polygone *)
	let equation_obstacle = fun obst -> let pt_lst = give_poly obst in
		match pt_lst with
			|[] -> []
			|[p] -> []
			|p1::p2::q -> (equation_droite p1 p2)::(equation_obstacle p2::q)

	(* Fonction qui donne les segmenst composant le polygone *)
	let segmente_obstacle = fun obst -> let pt_lst = give_poly obst in
		match pt_lst with
			|[] -> []
			|[p] -> []
			|p1::p2::q -> Segment(p1,p2)::(segmente_obstacle p2::q)

	(* Fonction qui donne le point d'intersection de deux droites *)
	let intersection_droites = fun d1 d2 -> 
		let a1,b1 = give_seg d1 in
		let a2,b2 = give_seg d2 in
		let m1,q1 = equation_droite a1 b1 in (*Equation de d1*)
		let m2,q2 = equation_droite a2 b2 in (*Equation de d2*)
		let x_cross = (q2 -. q1)/.(m1 -. m2) in
		Point({x=x_cross, y=m1 *. x_cross +. q1})

	(* Fonction qui indique si un point se situe dans un segment *)
	let pt_dans_seg = fun pt seg ->
		let p = give_pt pt in 
		let classe_coord = fun xa xb -> if (xa < xb) then xa,xb else xb,xa in (*Fonction qui donne les coordonnées de deux points dans l'ordre croissant*)
		let a_seg,b_seg = give_seg seg in
		let m,q = equation_droite a_seg b_seg in (*Equation de la droite associée au segment*)
		let sur_droite = (m *. p.x +. q -. p.y< epsilon) in (*Le point se situe sur la droite ?*)
		let x_min, x_max = classe_coord a_seg.x, b_seg.x in
		let y_min, y_max = classe_coord a_seg.y, b_seg.y in
		let intra_h = (p.x <= x_max) && (p.x >= x_min)in (*Le point se situe horizontalement entre les deux points ?*)
		let intra_v = (p.y <= y_max) && (p.y >= y_min)in (*Le point se situe verticalement entre les deux points ?*)
		sur_droite && intra_h && intra_v

	(* Fonction qui indique si deux segments se croisent *)
	let croise_segment = fun s1 s2 ->
		let pt_cross = intersection_droites s1 s2 in
		(pt_dans_seg pt_cross s1) && (pt_dans_seg pt_cross s2) (*2 segments se croisent si le point d'intersection des deux droites associées appartient aux deux segments*)

	(* Fonction qui indique si un segment traverse un obstacle *)
	let croise_obstacle = fun seg obst ->
		let lst_segments = segmente_obstacle obst in
		let rec f_aux = fun lst test -> (*On regarde si le segment croise un de ceux composant l'obstacle*)
			match lst,test with
				|[] -> test
				|_ when test -> test
				|t::q when not test -> f_aux q (croise_segment seg t)
		in f_aux lst_segments false

	(* Fonction qui indique si un segment de la trajectoire traverse un des obstacles *)
	let trajectoire_ok = fun traj lst_obst ->
		let rec segmente_traj = fun trj -> match trj with
			|[] -> []
			|[p] -> []
			|p1::p2::q -> Segment(p1,p2)::(segmente_traj p2::q)
		in
		let traj_seg = segmente_traj traj in 
		let rec ok_pour_un = fun lst_traj obst test ->
			match lst_traj with
				|[] -> test
				|_ when test -> test
				|t::q -> ok_pour_un q obst (croise_obstacle t obst) 
		in 
		let f_aux = fun lst_o traverse ->
			match lst_o with
				|[] -> traverse
				|_ when traverse -> traverse
				|t::q -> f_aux q (ok_pour_un traj_seg t false)
		in not f_aux lst_obst false

end;;
