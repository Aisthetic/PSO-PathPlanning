open Initialisation;;
open Initialisation_obstacles;;
open Gui;;
open Geometrie;;
open Utility_print;;


let pas = 1.;;
let nb_etats = 10;;
let p_obj = {x = 80.; y = 60.};;
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 10.;;


let obstacle1 = {sommets= [| {x=10.; y=10.};
							{x=20.; y=10.};
							{x=20.; y=20.};
							{x=10.; y=10.}	|];
				vitesse= (10.,10.)};;

let mvt = gen_etats [|obstacle1|] 1. 10;;


let sommets_dobstacle = fun obs ->
	obs.sommets;;



let swarm = gen_swarm 1 2 p_obj xmax vmax mvt 30. pas;;

print_swarm swarm;;

(*create mvt.(0).obstacles.sommets (float_array_to_point_array swarm.(0).position);;*)