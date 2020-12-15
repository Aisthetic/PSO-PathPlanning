open Geometrie
type point = Geometrie.point;;

let print_array_it = fun fl ->
	print_float fl;
	print_string "\n";;

let rec print_list = function 
	[] -> ()
	| e::l -> print_float e ; print_string " " ; print_list l

let print_array = function ar ->
	print_list (Array.to_list ar)

let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

let print_segment = fun s ->
    let p1, p2 = s in
    print_string "["; print_point p1; print_string " , "; print_point p2; print_string "]" ;;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->  (* particule is an array *)
	print_string "\nposition :\n" ; Array.iter print_array_it particule.position ; 
	print_string "vitesse :\n" ; Array.iter print_array_it particule.vitesse ; 
	print_string "meilleure position : \n" ; Array.iter print_array_it particule.meilleur ; print_string " ";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;

let print_obstacle = fun obstacle ->
	print_string "\nforme :\n" ; Array.iter print_array (Utility_array.point_array_to_floataa (Array.to_list obstacle.sommets));
	print_string "vitesse :\n" ; 
	match obstacle.vitesse with vx, vy -> print_string "("; print_float vx; print_string ","; print_float vy; print_string ")";;

let print_obstacles = fun obstacles ->
	Array.iter print_obstacle obstacles;;

let print_etat = fun etat ->
	print_string "\ntemps :"; print_float etat.t; print_string "\n";
	print_obstacles etat.obstacles;;

let print_etats = fun etats ->
	Array.iter print_etat etats;;

