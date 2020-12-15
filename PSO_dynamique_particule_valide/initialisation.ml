(* This module contains the functions needed to generate a swarm and to display it *)


type point = Geometrie.point;;
type particule = 
	{position : float array; 
	vitesse : float array; 
	meilleur : float array};;


type etat = Geometrie.etat;;


(* génération de la nouvelle seed aléatoire *)
Random.self_init ();;


(*pour aller plus loin : trouver une méthode pour ne pas faire appel aux array.append *)


(*transforme un flaot array en point list*)
let array_to_point tab = 
	let len = Array.length tab in 
	let lst = ref [] in
	let i = ref (len-2) in 
	while !i >= 0 do 
		lst := {Geometrie.x = tab.(!i); y = tab.(!i+1)} :: !lst;
		i := !i-2;
	done;
	!lst;;


(* génère un point aléatoire de dimension 2 avec p les coordonnées maximales *)
let gen_point = fun xmax ->
	[|Random.float xmax; Random.float xmax|]  (*REGARDER POUR TRONQUER UN FLOTTANT*)


(*Disponible dans algorithme; juste la pour developpement A SUPPRIMER *)
let float_array_to_point_array float_array =
	let len = Array.length float_array in
	let point_array = Array.make (len/2) {Geometrie.x=0.;y=0.} in
	let i = ref 0 in
	while !i < (len/2) do 
		point_array.(!i) <- {Geometrie.x=float_array.(2*(!i)); y=float_array.(2*(!i)+1)};
		i := !i + 1;
	done;
	point_array;;


let modulo_float = fun nombre diviseur ->
	let compt = ref 0 in
	let temp = ref nombre in
	while (!temp -. diviseur) > 0. do
		temp := !temp -. diviseur;
		compt := !compt + 1;
	done;
	!compt;;



let valide = fun last_point new_point obstacle_mvt vitesse_avion temps_passe pas ->
	let a = (float_array_to_point_array last_point).(0) in
	let b = (float_array_to_point_array new_point).(0) in
	let temps_traj = (Geometrie.distance a b) /. vitesse_avion in
	let t_tot = temps_passe +. temps_traj in
	let rang_depart = modulo_float temps_passe pas in
	let rang_arrivee = modulo_float t_tot pas in
	let distance_intermediaire_x = abs_float (a.x -. b.x) /. pas in
	let a_intermediaire = ref a in
	let b_intermediaire = ref {Geometrie.x = (!a_intermediaire.x +. b.x /. (float_of_int (rang_arrivee - rang_depart)));
										 y = (!a_intermediaire.y +. b.y /. (float_of_int (rang_arrivee - rang_depart)))} in
	let test = ref true in
	let i = ref rang_depart in
	while (!test && i<=rang_arrivee) do
		a_intermediaire := !b_intermediaire;
		b_intermediaire := {Geometrie.x = (!a_intermediaire.x +. b.x /. (float_of_int (rang_arrivee - rang_depart)));
										 y = (!a_intermediaire.y +. b.y /. (float_of_int (rang_arrivee - rang_depart)))};
		let sommets_dobstacle (obs : obstacle) : float list = obs.sommets; (* prend un obstacle et renvoie ses sommets *)
		test := (!test && (Geometrie.trajectoire_ok [!a_intermediaire;!b_intermediaire] (Array.map sommets_dobstacle obstacle_mvt.(!i).obstacles )));
		i := !i +1;
	done;
	!test;;





(* génère un array de nb points de dimension 2 avec p_obj les limites *)
(*Trajectoire commence forcément par 0,0 et termine par px,py*)
let generation_traj = fun nb xmax p_obj obstacle_mvt vitesse_avion pas-> (*Génère une trajectoire*)
	let rec rec_gen = fun trajectoire increment last_point->
		if increment = nb then Array.append trajectoire ([|p_obj.Geometrie.x; p_obj.y|])
		else
			let new_point = ref gen_point xmax in
			while not (valide last_point !new_point obstacle_mvt vitesse_avion temps_passe pas) do 
				new_point := gen_point xmax;
			done;
			rec_gen (Array.append trajectoire new_point) (increment+1) !gen_point
	in rec_gen ([| 0.; 0.|]) 0 [|0.;0.];;	







(* genère une vitesse *)
let generation_speed = fun nb vmax ->
	let rec rec_gen = fun speed increment ->
		if increment = nb then Array.append speed [|0.;0.|]
		else
			rec_gen (Array.append speed (gen_point vmax)) (increment+1)
	in rec_gen ([|0.;0.|]) 0;;







(* genère une particule*)
let gen_particule = fun nb p_obj xmax vmax obstacle_mvt vitesse_avion pas->
	let pos_init = (generation_traj nb xmax p_obj obstacle_mvt vitesse_avion pas) in
	{position = pos_init; vitesse = generation_speed nb vmax; meilleur = pos_init};;









(* génère un nombre n de particules avec nb points de dimension 2 et p_obj l'objectif, obstacle_mvt un deplacement list*)
let gen_swarm = fun n nb p_obj xmax vmax obstacle_mvt vitesse_avion pas->
	let rec rec_swarm = fun swarm increment ->
		if increment = n then swarm
		else
			rec_swarm (Array.append swarm [|(gen_particule nb p_obj xmax vmax obstacle_mvt vitesse_avion pas)|]) (increment+1)
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