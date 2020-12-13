open Geometrie
type point = Geometrie.point;;

let print_array = fun fl ->
	print_float fl;
	print_string "\n";;

let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

let print_segment = fun s ->
    let p1, p2 = s in
    print_string "["; print_point p1; print_string " , "; print_point p2; print_string "]" ;;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->  (* particule is an array *)
	print_string "\nposition :\n" ; Array.iter print_array particule.Initialisation.position ; 
	print_string "vitesse :\n" ; Array.iter print_array particule.vitesse ; 
	print_string "meilleure position : \n" ; Array.iter print_array particule.best_pos ; print_string " ";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;

let print_obstacle = fun obstacle ->
	print_string "\nforme :\n" ; Array.iter print_array obstacle.Initialisation.aretes ;
	print_string "vitesse :\n" ; print_float obstacle.vitesse ;;

let print_obstacles = fun obstacles ->
	Array.iter print_obstacle obstacles;;
