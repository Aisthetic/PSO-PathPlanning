let n = 1;; 					(*Nombre de particules*) 
let nb_pts_tournants = 2;; 		(*Nombre de points tournants*) 
let d = 2*nb_pts_tournants;; 	(*Taille des tableaux dans particules*)
let iterMax = 100;; 			(*Nombre d'itération max*)
let epsilon = 0.1;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)

type particule = {position: float array; vitesse: float array; meilleur: float array};; 

let genere_particules n = 					(*Fonction qui génère un tableau de n particules, faite par Marc*)	
	[| {	position = [|0.|]; 	
			vitesse = [|0.|]; 
			meilleur= [|0.|] }	|];; 


(*let soustrait_points pt1 pt2 = {x=pt1.x -. pt2.x; y=pt1.y -. pt2.y};;
let somme_points pt1 pt2 = {x=pt1.x +. pt2.x; y=pt1.y +. pt2.y};;
let mult_point pt coeff = {x=coeff *. pt.x; y=coeff *. pt.y};;
let distance pt1 pt2 = 0.;; (*Fonction def dans Geometry*) *)

let fonction_objectif float_tab = 0.;; 		(*Fonction à minimiser*)

let maj_vitesse part g = 					(*Calcul de la nouvelle vitesse à partir de la formule*)
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in
	for i=0 to (d-1) do 
		part.vitesse.(i)<- w *. v.(i) +. c1 *. (m.(i) -. p.(i)) +. c2 *. (g.(i) -. p.(i)); 
	done;;

let maj_meilleur_local part = 				(*Compare la nouvelle position calculée au meilleur local*)
	if fonction_objectif (part.position) < fonction_objectif (part.meilleur) then
	begin (*On remplace le meilleur local*)
		for i=0 to (d-1) do
			part.meilleur.(i) <- part.position.(i);
		done;
	end;;

let maj_position_et_local part = 			(*On a maj la vitesse, il faut déplacer la particule*)
	for i=0 to (d-1) do 
		part.position.(i) <- part.position.(i) +. part.vitesse.(i);(*maj position*)
		if part.position.(i) > xmax then (*Les points qui sortent sont mis sur le bord et on annule leur vitesse*)
			begin
				part.position.(i) <- xmax; 
				part.vitesse.(i) <- 0.;
			end 	
	done; 
	maj_meilleur_local part;;

let moyenne_float_array tab =				(*Fonction qui calcule la moyenne d'un tableau de float*) 
	let len = Array.length tab in
	let moy = ref(0.) in
	for i=0 to (len-1) do 
		moy := !moy +. tab.(i)
	done;
	!moy /. float_of_int(len);;

let maj_vitesse_moy particules =
	let v_moy = Array.make n 0. in
	for i=0 to (n-1) do
		v_moy.(i) <- moyenne_float_array particules.(i).vitesse;
	done;
	moyenne_float_array v_moy;;


let maj_meilleur_global particules g = 		(*Compare les meilleurs locaux au global*)
	let nouveau_g = ref(g) in
	for i=0 to n do
		if (fonction_objectif (particules.(i).meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)



let algo = fun _ -> 						(*Algo à lancer dans le main*)
	let particules = genere_particules n in 				
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g->
		if ((cmpt > iterMax) || (var_vitesse < epsilon)) then g
		else 
			begin
				for i=0 to (n-1) do 
					maj_vitesse particules.(i) g; 					(*Maj vitesse*) 
					maj_position_et_local particules.(i); 			(*Maj position et meilleur local*) 
				done;
				let nvl_vitesse_moy = maj_vitesse_moy particules in	(*Pour tester si on stagne*)
				let nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy) in 
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy (maj_meilleur_global particules g);
			end
	in particleswarm particules 0 1. 3. particules.(0).meilleur;;	(*Checker valeurs initiales*)