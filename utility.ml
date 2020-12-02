open Geometrie;;

type point= Geometrie.point;;

let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

let print_segment = fun s ->
    let p1, p2 = s in
    print_string "["; print_point p1; print_string " , "; print_point p2; print_string "]" ;;

(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "position :\n" ; Array.iter print_point particule.position ; print_string "\nvitesse :\n" ; Array.iter print_point particule.vitesse ; print_string "\nmeilleure position : \n" ; Array.iter print_point particule.best_pos ; print_string " \n\n";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;
