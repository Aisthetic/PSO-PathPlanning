(* This module contains the main path planning 
 * algorithm that can be called by other modules
.*)

type point = {x : float; y : float};; (* définit dans géometry et à importer si besoin *)



(* les points mde départ et d'arrivée' *)
let p_init = (0., 0.);;     
let p_final = (100., 100.);;

(* le nombre de points tournant *)
let nb_pt = 3;;


(* sert pour test fonction de génération, à remplacer par une génération aléatoire *)
let randx = 5.;;
let randy = 6.;;

(*let generation_particule = fun max ->
	let particule = [] in
	for ident = 1 to max do
		let new_point = [{x = randx; y = randy}] in                                  (* borner l'ensemble de genération d'un point en fonction du précedent pour eviter un retour en arrière trop important *)
		new_point :: particule;
	done;;*)

let generation = fun max->
	let rec rec_gen = fun particule increment ->
		if increment = max then particule 
		else
			rec_gen ({x = randx; y = randy} :: particule) (increment+1)
	in rec_gen [] 0;;	

(*let particule1 = generation_particule nb_pt;*)
let particule2 = generation nb_pt;;
Printf.fprintf (string particule2);;
