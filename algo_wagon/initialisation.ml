(* This module contains the functions needed to generate a swarm and to display it *)

type particule = {position : float array; vitesse : float array; best_pos : float array};;

type obstacle_wrapper = {obstacle : float array; vitesse : float};; (*  *)

 (* génération de la nouvelle seed aléatoire *)
 Random.self_init ();;

(* génère un point aléatoire de dimanesion k avec p les coordonnées maximales *)
let gen_point = fun k p ->
	let rec rec_gen_point = fun point dimension ->
		if dimension = k then point
		else
			rec_gen_point (Array.append point [| Random.float p.(dimension) |]) (dimension +1)
	in rec_gen_point [||] 0;;
	(*{x = Random.float p.x; y = Random.float p.y};;*)

(* gènère un array de nb points de dimension k avec p les limites *)
let generation = fun nb k p->
	let rec rec_gen = fun particule increment ->
		if increment = nb then particule 
		else
			rec_gen (Array.append particule  (gen_point k p)) (increment+1)
	in rec_gen [||] 0;;	

(* génère un nombre d de particules avec nb points de dimension k et p les limites*)
let  gen_swarm = fun d nb k p ->
	let rec rec_swarm = fun swarm increment ->
		if increment = d then swarm
		else
			let pos_init = generation nb k p in
			rec_swarm (Array.append swarm [| {position = pos_init; vitesse = generation nb k p; best_pos = pos_init} |]) (increment+1)
	in rec_swarm [||] 0;;

(* génère un obstacle aléatoire avec N points de dimension k avec p les coordonnées maximales *)
let gen_obstacle = fun nombre_obstacle dimension coord_max vitesse_max ->
	 {obstacle = (generation nombre_obstacle dimension coord_max) ; vitesse = Random.float vitesse_max};;

(* génère nombre_obs fois d'obstacles aléatoires avec N points de dimension k avec p les coordonnées maximales *)
let gen_obstacles = fun nombre_obs dimension coord_max vitesse_max nombre_obstacle_max ->
	let rec rec_gen_obs = fun obstacles nb_obs ->
		if nb_obs = nombre_obs then obstacles
		else
			let nb_obstacle = Random.int nombre_obstacle_max in
			rec_gen_obs (Array.append obstacles [| gen_obstacle nb_obstacle dimension coord_max vitesse_max |]) (nb_obs +1)
	in rec_gen_obs [||] 0;;	
	
(* AFFICHAGE *)

(* permet l'affichage de coordonnees en float *)
let print_array = fun fl ->
	print_float fl;
	print_string "\n";;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "\nposition :\n" ; Array.iter print_array particule.position ; print_string "vitesse :\n" ; Array.iter print_array particule.vitesse ; print_string "meilleure position : \n" ; Array.iter print_array particule.best_pos ; print_string " ";;
	

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;
	print_string "\n";;





