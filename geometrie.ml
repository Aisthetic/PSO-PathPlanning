type point = {x : float; y : float};;	

exception DroitesParalleles;;

exception DroiteVerticale;;

(* Donne la différence acceptable entre deux flottants *)
let epsilon = 10.**(-5.);;

(* Fonction qui donne la distance entre deux points *)
let distance = fun a b -> sqrt (((a.x -. b.x)**2.) +. ((a.y -. b.y)**2.));;

(*let fonction_objectif = fun traj ->
	let rec aux = fun lst acc ->
		match lst with
			|[] -> acc
			|(a,b)::q -> aux q (acc +. (distance a b))
	in aux traj 0.;;

;; Juste pour pas avoir d'erreur, prendre fonction Caro*)

(*let fonction_objectif = fun traj ->
	let n = Array.length traj in
	let d = ref 0. in 
	for i = 0 to (n-1) do 
		let a,b = traj.(i) in
		d := !d +. (distance a b)
	done; 
	!d;;*)

let mult_point pt coeff = {x=coeff *. pt.x; y=coeff *. pt.y};;

let soustrait_points pt1 pt2 = {x=pt1.x -. pt2.x; y=pt1.y -. pt2.y};;

let somme_points pt1 pt2 = {x=pt1.x +. pt2.x; y=pt1.y +. pt2.y};;

(* Fonction qui donne les paramètres d'une droite connaissant deux de ses points *)
let equation_droite = fun a b -> 
	let dx = (b.x -. a.x) in
	 let dy = (b.y -. a.y) in 
	 (if dx = 0. then 0. else dy /. dx), (a.y -. ((dy /. dx) *. a.x));;

(* Fonction qui donne les paramètres des droites formant le polygone *)
let rec equation_obstacle = fun obst ->
	match obst with
		|[] -> []
		|[p] -> []
		|p1::p2::q -> (equation_droite p1 p2)::(equation_obstacle (p2::q));;

(* Fonction qui donne les segments composant le polygone *)
let rec segmente_obstacle = fun obst ->
	match obst with
		|[] -> []
		|[p] -> []
		|p1::p2::q -> (p1,p2)::(segmente_obstacle (p2::q));;

(* Fonction qui donne le point d'intersection de deux droites *)
let intersection_droites = fun d1 d2 -> 
	let a1,b1 = d1 in
	let a2,b2 = d2 in
	let m1,q1 = equation_droite a1 b1 in
	let m2,q2 = equation_droite a2 b2 in
	let dm = m2 -. m1 in 
	let dq = q2 -.q1 in
	if dm = 0. then raise DroitesParalleles else {x= dq/.dm;y= m1*.(dq/.dm) +. q1}
;;

(* Fonction qui indique si un point se situe dans un segment *)
let pt_dans_seg = fun pt seg ->
	let pt1,pt2 = seg in
	(pt1.x < pt.x && pt.x < pt2.x || pt1.x > pt.x && pt.x > pt2.x) && (pt1.y < pt.y && pt.y < pt2.y || pt1.y > pt.y && pt.y > pt2.y)
;;

(* Fonction qui indique si deux segments se croisent *)
let croise_segment = fun s1 s2 ->
	try 
	let pt_cross = intersection_droites s1 s2 in
	(pt_dans_seg pt_cross s1);
	with DroitesParalleles -> false
;;(*2 segments se croisent si le point d'intersection des deux droites associées appartient aux deux segments*)

(* Fonction qui indique si un segment traverse un obstacle *)
let croise_obstacle = fun seg obst ->
	let lst_segments = segmente_obstacle obst in
	let rec f_aux = fun lst test -> (*On regarde si le segment croise un de ceux composant l'obstacle*)
		match lst with
			|[] -> test
			|t::q when test ->  test
			|t::q -> let tst = (croise_segment seg t) in f_aux q tst
	in f_aux lst_segments false;;

(* Fonction qui indique si un segment de la trajectoire traverse un des obstacles *)
let trajectoire_ok = fun traj lst_obst ->
	let rec segmente_traj = fun trj -> match trj with
		|[] -> []
		|[p] -> []
		|p1::p2::q -> (p1,p2)::(segmente_traj (p2::q))
	in
	let traj_seg = segmente_traj traj in 
	let rec ok_pour_un = fun lst_traj obst test ->
		match lst_traj with
			|[] -> test
			|t::q when test -> test
			|t::q -> ok_pour_un q obst (croise_obstacle t obst) 
	in 
	let rec f_aux = fun lst_o traverse ->
		match lst_o with
			|[] -> traverse
			|_ when traverse -> traverse
			|t::q -> f_aux q (ok_pour_un traj_seg t false)
	in not (f_aux lst_obst false);;
