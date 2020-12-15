(* This module contains the functions needed to generate a swarm and to display it *)
open Geometrie

type point = Geometrie.point;;
type particule = Geometrie.particule;;


(* génération de la nouvelle seed aléatoire *)
Random.self_init ();;


(*pour aller plus loin : trouver une méthode pour ne pas faire appel aux array.append *)
(* génère un point aléatoire de dimaesion 2 avec p les coordonnées maximales *)
let gen_point = fun xmax ->
	[|Random.float xmax; Random.float xmax|]  (*REGARDER POUR TRONQUER UN FLOTTANT*)

(* génère un array de nb points de dimension 2 avec p_obj les limites *)
(*Trajectoire commence forcément par 0,0 et termine par px,py*)

let generation_traj = fun nb xmax p_obj-> (*Génère une trajectoire*)
	let rec rec_gen = fun trajectoire increment ->
		if increment = nb then Array.append trajectoire ([|p_obj.x; p_obj.y|])
		else
			rec_gen (Array.append trajectoire (gen_point xmax)) (increment+1)
	in rec_gen ([| 0.; 0.|]) 0;;	

(* genère une vitesse *)
let generation_speed = fun nb vmax ->
	let rec rec_gen = fun speed increment ->
		if increment = nb then Array.append speed [|0.;0.|]
		else
			rec_gen (Array.append speed (gen_point vmax)) (increment+1)
	in rec_gen ([|0.;0.|]) 0;;



(* genère une particule*)
let gen_particule = fun nb p_obj xmax vmax obstacle->
	let pos_init = ref (generation_traj nb xmax p_obj) in
	while not (Geometrie.trajectoire_ok (Utility_array.array_to_point !pos_init) obstacle) do 
		pos_init := (generation_traj nb xmax p_obj)
	done;
	{position = (Utility_print.copie_tab !pos_init); vitesse = generation_speed nb vmax; meilleur = (Utility_print.copie_tab !pos_init)};; 


(* génère un nombre n de particules avec nb points de dimension 2 et p_obj l'objectif, obstacle une point list list*)
let gen_swarm = fun n nb p_obj xmax vmax obstacle->
	let rec rec_swarm = fun swarm increment ->
		if increment = n then swarm
		else
			rec_swarm (Array.append swarm [|(gen_particule nb p_obj xmax vmax obstacle)|]) (increment+1)
	in rec_swarm [||] 0;;



(* AFFICHAGE *)

(* permet l'affichage de coordonnees en float *)
let print_array = fun fl ->
	print_float fl;
	print_string "\n";;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "\nposition :\n" ; 
	Array.iter print_array particule.position ; 
	print_string "vitesse :\n" ; 
	Array.iter print_array particule.vitesse ; 
	print_string "meilleure position : \n" ; 
	Array.iter print_array particule.meilleur ; 
	print_string " ";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;



(* (* ZONE DE TESTS EN TRAVAUX*)
let n = 5;; 					(*Nombre de particules*) 
let nb = 3;;					(*Nombre de points*)
let d = 2 * nb;; 				(*Taille des tableaux dans particules*)
let iterMax = 100;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 10.;;					(*Vitesse max d'une particule*)
let infini = 1000000.	;;		(*Infini*)
let p_obj = {x = 80.; y = 60.};;(*Destination*) 
let obstacle = [[	{x=30.; y= 15.}; 
					{x=70.; y=50.}; 
					{x=34.; y= 52.}; 
					{x=30.; y= 15.}]; 
				[	{x=10.; y= 15.};
					{x=25.; y= 15.};
					{x=25.; y= 50.};
					{x=10.; y= 50.}; 
					{x=10.; y= 15.};]];;
(* let obstacle =[[]];;
 *)


print_swarm (gen_swarm n nb p_obj xmax vmax obstacle);; *)