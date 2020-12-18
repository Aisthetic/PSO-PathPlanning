open Initialisation;;
open Initialisation_obstacles;;
open Gui;;
open Geometrie;;
open Utility_print;;
open Utility_array;;


let pas = 1.;;
let nb_etats = 1000;;
let p_obj = {x = 80.; y = 60.};;
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 10.;;


let obstacle1 = {sommets= [ {x=10.; y=10.};
							{x=20.; y=10.};
							{x=20.; y=20.};
							{x=10.; y=10.}	];
				vitesse= (10.,10.)};;

let mvt = gen_etats [obstacle1] 1. 10;;
(* print_etats mvt;; *)

let conv mvt = Array.map Array.of_list (Array.of_list mvt.obstacles);;
let obs_animated = Array.map conv mvt;;

let swarm = gen_swarm 1 2 p_obj xmax vmax mvt 20. 1.;;
let swarm_animated = Array.make (Array.length obs_animated) (float_array_to_point_array swarm.(0).position);;
print_swarm swarm;;
Gui.create_animated obs_animated swarm_animated 1.0;;
