(* This module contains the main path planning 
 * algorithm that can be called by other modules
.*)



type point = {x : float; y : float};; (* définit dans géometry et à importer si besoin *)
type particule = {position : point array; vitesse : point; best_pos : point array};;



(* les points mde départ et d'arrivée pas encore utilisés en fait *)
let p_init = {x = 0.; y = 0.};;     
let p_final = {x = 100.; y = 100.};;

(* le nombre de points tournant *)
let nb_pt = 3;;

(* le nombre de particules désirées *)
let d = 2;;
 
 (* génération de la nouvelle seed aléatoire *)
 Random.self_init ();;


let gen_point = fun p ->
	{x = Random.float p.x; y = Random.float p.y};;

(* gènère un array de max points  *)
let generation = fun max->
	let rec rec_gen = fun particule increment ->
		if increment = max then particule 
		else
			rec_gen (Array.append particule [| gen_point p_final |]) (increment+1)
	in rec_gen [||] 0;;	

(* génère un nombre d de particules *)
let  gen_swarm = fun d max->
	let rec rec_swarm = fun swarm increment ->
		if increment = d then swarm
		else
			let pos_init = generation max in
			rec_swarm (Array.append swarm [| {position = pos_init; vitesse = gen_point p_final; best_pos = pos_init} |]) (increment+1)
	in rec_swarm [||] 0






(* permet l'affichage d'un point *)
let print_point = fun p ->
	print_string "(" ; print_float p.x ; print_string ", " ; print_float p.y ; print_string ")" ; print_string " ";;	


(* permet l'affichage d'une liste de points *)
let print_array = fun liste ->
	Array.iter print_point liste;
	print_string "\n";;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "\nposition :\n" ; print_array particule.position ; print_string "vitesse : \n" ; print_point particule.vitesse ; print_string "\nmeilleure position : \n" ; print_array particule.best_pos ; print_string " ";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;


print_swarm (gen_swarm d nb_pt);;

