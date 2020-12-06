(* This module contains the functions needed to generate a swarm and to display it *)
open Geometrie

type point = Geometrie.point;;
type particule = 
	{position : float array; 
	vitesse : float array; 
	meilleur : float array};;


(* génération de la nouvelle seed aléatoire *)
Random.self_init ();;

let array_to_point tab = 
	let len = Array.length tab in 
	let lst = ref [] in
	let i = ref (len-2) in 
	while !i >= 0 do 
		lst := {x = tab.(!i); y = tab.(!i+1)} :: !lst;
		i := !i-2;
	done;
	!lst;;


(* génère un point aléatoire de dimaesion 2 avec p les coordonnées maximales *)
let gen_point = fun p ->
	[|Random.float p.x; Random.float p.y|]

(* génère un array de nb points de dimension 2 avec p_obj les limites *)
(*Trajectoire commence forcément par 0,0 et termine par px,py*)
let generation = fun nb p_obj-> (*Génère une trajectoire*)
	let rec rec_gen = fun trajectoire increment ->
		if increment = nb then Array.append trajectoire ([|p_obj.x; p_obj.y|])
		else
			rec_gen (Array.append trajectoire (gen_point p_obj)) (increment+1)
	in rec_gen ([| 0.; 0.|]) 0;;	

(* génère un nombre n de particules avec nb points de dimension 2 et p_obj les limites, obstacle une point list list*)
let gen_swarm = fun n nb p_obj obstacle->
	let rec rec_swarm = fun swarm increment ->
		if increment = n then swarm
		else
			let pos_init = generation nb p_obj in
			if (Geometrie.trajectoire_ok (array_to_point pos_init) obstacle) then
				rec_swarm (Array.append swarm [| {position = pos_init; vitesse = generation nb p_obj; meilleur = pos_init} |]) (increment+1)
			else
				rec_swarm swarm increment
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



(* ZONE DE TESTS EN TRAVAUX*)
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
let vmax = 3.;;					(*Vitesse max d'une particule*)
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


print_swarm (gen_swarm n nb p_obj obstacle);;