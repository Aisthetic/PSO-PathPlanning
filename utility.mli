type point = Geometrie.point


val print_point : point -> unit
val print_point : point * point -> unit

(* 
(* permet l'affichage d'une particule *)
let print_particule = fun particule ->
	print_string "position :\n" ; Array.iter print_point particule.position ; print_string "\nvitesse :\n" ; Array.iter print_point particule.vitesse ; print_string "\nmeilleure position : \n" ; Array.iter print_point particule.best_pos ; print_string " \n\n";;

(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";; 
    *)
