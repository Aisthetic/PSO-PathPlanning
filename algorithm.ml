(* This module contains the main path planning 
 * algorithm that can be called by other modules
.*)

type point = {x : float; y : float} (* définit dans géometry et à importer si besoin *)

type particule = point list

(* les points mde départ et d'arrivée' *)
let P_init = (0., 0.)     
let P_final = (100., 100.)

(* le nombre de points tournant *)
let nb_pt = 3


(* sert pour test fonction de génération, à remplacer par une génération aléatoire *)
let randx = 5.
let randy = 6.

let generation_particule = fun NB_PT ->
	let particule = [] in
	for ident = 1 to NB_PT do
		let new_point = [x = randx; y = randy] in                                  (* borner l'ensemble de genération d'un point en fonction du précedent pour eviter un retour en arrière trop important *)
		List.append particule new_point
	done

let generation = fun NB_PT->
	let rec rec_gen = fun particule increment ->
		if increment = NB_PT then particule 
		else
			rec_generation (List.append particule [x = randx; y = randy]) increment+1
	rec_gen [] 0


let particule1 = generation_particule nb_pt
let particule2 = generation nb_pt