(* This module contains the functions needed to generate a swarm and to display it *)
open Geometrie



type point = Geometrie.point;;
type particule = {position : point array; vitesse : point array; best_pos : point array};;




 
(* génération de la nouvelle seed aléatoire *)
Random.self_init ();;

(* génère un point aléatoire de dimaesion 2 avec p les coordonnées maximales *)
let gen_point = fun p ->
	{x = Random.float p.x; y = Random.float p.y};;

(* génère un array de nb points de dimension 2 avec p les limites *)
let generation = fun nb p->
	let rec rec_gen = fun particule increment ->
		if increment = nb then Array.append particule [|{x=p.x;y=p.y}|]
		else
			rec_gen (Array.append particule  [|(gen_point p)|] ) (increment+1)
	in rec_gen [|{x=0.;y=0.}|] 0;;	

(* génère un nombre d de particules avec nb points de dimension 2 et p les limites, obstacle une point list list*)
let  gen_swarm = fun d nb p obstacle->
	let rec rec_swarm = fun swarm increment ->
		if increment = d then swarm
		else
			let pos_init = generation nb p in
			if (Geometrie.trajectoire_ok (Array.to_list pos_init) obstacle) then
				rec_swarm (Array.append swarm [| {position = pos_init; vitesse = generation nb p; best_pos = pos_init} |]) (increment+1)
			else
				rec_swarm swarm increment
	in rec_swarm [||] 0;;



(* AFFICHAGE *)

(* permet l'affichage de coordonnees en float *)
let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "position :\n" ; Array.iter print_point particule.position ; print_string "\nvitesse :\n" ; Array.iter print_point particule.vitesse ; print_string "\nmeilleure position : \n" ; Array.iter print_point particule.best_pos ; print_string " \n\n";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;
