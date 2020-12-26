type point = {x : float; y : float};;	
type trajectoire = 
	|In of point list * trajectoire
	|Out of point list * trajectoire
	|End of point;;

exception DroitesParalleles;;

exception DroiteVerticale;;

(* Donne la différence acceptable entre deux flottants *)
let epsilon = 10.**(-5.);;

(* Fonction qui donne la distance entre deux points *)
let distance = fun a b -> sqrt (((a.x -. b.x)*.(a.x -. b.x)) +. ((a.y -. b.y)*.(a.y -. b.y)));;
 


(* POUR DEBUGGAGE A SUPPRIMER ENSUITE *)
let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

(*Affiche une liste de point*)
let rec print_point_list pt_list = match pt_list with
	|[] -> print_string "\nfin\n"
	|p::q -> 	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ")\n ";
				print_point_list q;;





(* Fonction qui donne les paramètres d'une droite connaissant deux de ses points *)
let equation_droite = fun a b -> 
	if ((abs_float (a.x -. b.x)) < epsilon) then raise DroiteVerticale
	else
		let m = (b.y -. a.y)/.(b.x -. a.x) in m, (a.y -. (m *. a.x));;

(* Fonction qui donne les paramètres des droites formant le polygone *)
let rec equation_obstacle = fun obst ->
	match obst with
		|[] -> []
		|[p] -> []
		|p1::p2::q -> (equation_droite p1 p2)::(equation_obstacle (p2::q));;

(* Fonction qui donne les segmenst composant le polygone *)
let rec segmente_obstacle = fun obst ->
	match obst with
		|[] -> []
		|[p] -> []
		|p1::p2::q -> (p1,p2)::(segmente_obstacle (p2::q));;

let parallele = fun d1 d2 ->
	let a,b = d1 in
	let c,d = d2 in 
	if (((abs_float (a.x -. b.x)) < epsilon) && ((abs_float (c.x -. d.x)) < epsilon)) then true
	else begin
		if (((abs_float (a.x -. b.x)) < epsilon) || ((abs_float (c.x -. d.x)) < epsilon)) then false
		else begin
			let m1,q1 = equation_droite a b in
			let m2,q2 = equation_droite c d in
			(abs_float (m1 -. m2)) < epsilon;
		end
	end
;;

(* Fonction qui donne le point d'intersection de deux droites *)
let intersection_droites = fun d1 d2 -> 
	let a1,b1 = d1 in
	let a2,b2 = d2 in
	if parallele d1 d2 then raise DroitesParalleles
	else
		if ((abs_float (a1.x -. b1.x)) < epsilon) then begin
				let x_cross = a1.x in
				let m2,q2 = equation_droite a2 b2 in
				{x = x_cross ; y = (m2 *. x_cross) +. q2}; 
			end else
				if ((abs_float (a2.x -. b2.x)) < epsilon) then begin
						let x_cross = a2.x in 
						let m1,q1 = equation_droite a1 b1 in
						{x = x_cross ; y = (m1 *. x_cross) +. q1}; 
					end else 
						let m1,q1 = equation_droite a1 b1 in (*Equation de d1*)
						let m2,q2 = equation_droite a2 b2 in (*Equation de d2*)
						let x_cross = (q2 -. q1)/.(m1 -. m2) in
						{x = x_cross ; y = (m1 *. x_cross) +. q1};
;;

(* Fonction qui indique si un point se situe dans un segment *)
let pt_dans_seg = fun pt seg ->
	let classe_coord = fun xa xb -> if (xa < xb) then xa,xb else xb,xa in (*Fonction qui donne les coordonnées de deux points dans l'ordre croissant*)
	let a_seg,b_seg = seg in
	let sur_droite = 
		if ((abs_float (a_seg.x -. b_seg.x)) < epsilon) then (abs_float (a_seg.x -. pt.x ))< epsilon
		else
			let m,q = equation_droite a_seg b_seg in (*Equation de la droite associée au segment*)
			let res = m *. pt.x +. q -. pt.y in
			((abs_float res) < epsilon)(*Le point se situe sur la droite ?*)
	in
	let x_min, x_max = classe_coord a_seg.x b_seg.x in
	let intra_h = ((pt.x <= x_max) && (pt.x >= x_min)) in (*Le point se situe horizontalement entre les deux points ?*)
	let y_min, y_max = classe_coord a_seg.y b_seg.y in
	let intra_v = ((pt.y <= y_max) && (pt.y >= y_min)) in (*Le point se situe verticalement entre les deux points ?*)
	(sur_droite && intra_h && intra_v);;

(* Fonction qui indique si deux segments se croisent *)
let croise_segment = fun s1 s2 ->
	try 
		begin
			let pt_cross = intersection_droites s1 s2 in
			if ((pt_dans_seg pt_cross s1) && (pt_dans_seg pt_cross s2)) 
			then (true,pt_cross)
			else (false,{x=0.;y=0.}) (*Le point sera jamais lu car croise pas*)
		end
	with DroitesParalleles -> (false,{x=0.;y=0.})(*Le point sera jamais lu car croise pas*)
;;(*2 segments se croisent si le point d'intersection des deux droites associées appartient aux deux segments*)





let rec print_s_lst = fun s_lst ->
	match s_lst with
		|[] -> print_string "Fin \n\n"
		|t::q -> let a,b = t in Printf.printf "Segment : [ (%f, %f), (%f, %f) ]" a.x a.y b.x b.y; print_s_lst q
	;;




let sort_by_time lst seg = 
	let croiss_x ptA ptB = int_of_float (ptA.x -. ptB.x) in
	let croiss_y ptA ptB = int_of_float (ptA.y -. ptB.y) in
	let desc_x ptA ptB = int_of_float (ptB.x -. ptA.x) in
	let desc_y ptA ptB = int_of_float (ptB.y -. ptA.y) in
	let xA = (fst seg).x in
	let yA = (fst seg).y in
	let xB = (snd seg).x in
	let yB = (snd seg).y in
	if xA < xB then List.sort croiss_x lst
	else 
		begin
			if xA > xB then List.sort desc_x lst
			else 
				begin
					if yA < yB then List.sort croiss_y lst
					else List.sort desc_y lst
				end
		end 

(* Fonction qui renvoie les points qui croisent un obstacle*)
let croise_obstacle = fun seg obst ->
	let rec f_aux = fun lst_obst lst_cross -> (*On regarde si le segment croise un de ceux composant l'obstacle*)
		match lst_obst with
			|[] -> lst_cross
			|[h1] -> lst_cross
			|h1::h2::q -> 	let b,pt_cross = (croise_segment seg (h1,h2)) in
							if b then f_aux (h2::q) (lst_cross@[pt_cross])
							else f_aux (h2::q) lst_cross
			(* |_ -> failwith "Erreur croise_obstacle" *)
	in 
	let lst_cross = f_aux obst [] in 
	sort_by_time lst_cross seg;;


let print_seg = fun s ->
	let a,b = s in
	let xa = a.x in
	let ya = a.y in
	let xb = b.x in
	let yb = b.y in
	Printf.printf "[ (%f,%f) , (%f,%f) ]\n" xa ya xb yb
;;

type in_out = In|Out;;

let construit_traj chemin lst_obst = (*chemin liste de point*)
	let rec croise_all_obstacle lst_obst lst_cross seg = match lst_obst with
		|[] -> lst_cross
		|obst::q -> croise_all_obstacle q ((croise_obstacle seg obst)@lst_cross) seg
	in (*ICI QU'ON DOIT TRIER*)
	let rec aux chemin lst_obj traj in_Or_out = match chemin with
		|[] -> traj
		|p1::p2::q -> let lst_cross = sort_by_time (croise_all_obstacle lst_obst [] (p1,p2)) (p1,p2) in
						let rec bcle lst traj in_out = match lst with
							|[] -> (traj,in_out)
							|[x1] when in_out = Out -> 
								(traj@[(x1,Out);(x1,In)],In)
							|[x1] when in_out = In -> 
								(traj@[(x1,In);(x1,Out)],Out)
							|x1::x2::q when in_out = Out ->
								let nvl_traj = [(x1,Out);(x1,In);(x2,In);(x2,Out)] in
								bcle q (traj@nvl_traj) Out
							|x1::x2::q when in_out = In ->
								let nvl_traj = [(x1,In);(x1,Out);(x2,Out);(x2,In)] in 
								bcle q (traj@nvl_traj) In
							|_ -> failwith "chelou"
						in 
						let t,in_out = (bcle lst_cross [] in_Or_out) in				
						aux (p2::q) lst_obj (traj@(p1,in_Or_out)::t) in_out
		|[p1]-> traj@[(p1,Out)] (*p1=p_obj*)
	in aux chemin lst_obst [] Out;;

let rec print_traj traj = 
	match traj with
	|[] -> ()
	|(p1,inout)::q when inout = In -> Printf.printf "(%f,%f) IN \n" p1.x p1.y; print_traj q;
	|(p1,inout)::q when inout = Out -> Printf.printf "\n(%f,%f) OUT \n" p1.x p1.y; print_traj q;
	|_ -> failwith "erreur print traj"

(* let orig = {x= 0.;y= 0.};;
let p1 = {x=40.;y= 0.};;
let p2 = {x=40.;y=15.};;
let p3 = {x=20.;y=15.};;
let p4 = {x=20.;y=40.};;
let dest = {x=50.;y=20.};;
let a = {x=10.;y=10.};;
let b = {x=30.;y=10.};;
let c = {x=30.;y=30.};;
let d = {x=10.;y=30.};;

let chemin = [orig;p1;p2;p3;p4;dest];;
let obstacles = [[a;b;c;d;a]];; *)

(* 
print_traj (construit_traj chemin obstacles);; *)