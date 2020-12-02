let n = 5;; 					(*Nombre de particules*) 
let d = 3;; 					(*Taille des tableaux dans particules*)
let iterMax = 100;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let infini = 1000000.			(*Infini*)

type particule = {position: float array; vitesse: float array; meilleur: float array};; 

let genere_particules n = 					(*Fonction qui génère un tableau de n particules, faite par Marc*)	
	[| 	{	position = [|0.31; 0.13; 0.31|]; 	
			vitesse  = [|0.1 ; 0.12; 0.15|]; 
			meilleur = [|0.31; 0.13; 0.31|]		};
		{	position = [|0.51; 0.51; 0.12|]; 	
			vitesse  = [|0.08; 0.51; 0.11|]; 
			meilleur = [|0.51; 0.51; 0.12|] 	};
		{	position = [|0.34; 0.32; 0.13|]; 	
			vitesse  = [|0.1 ; 0.2 ; 0.15|]; 
			meilleur = [|0.34; 0.32; 0.13|]		};
		{	position = [|0.1 ; 0.2 ; 0.15|]; 	
			vitesse  = [|0.17; 0.02; 0.15|]; 
			meilleur = [|0.1 ; 0.2 ; 0.15|]		};
		{	position = [|0.13; 0.03; 0.13|]; 	
			vitesse  = [|0.19; 0.21; 0.05|]; 
			meilleur = [|0.13; 0.03; 0.13|]		};	|];; 

let fonction_objectif float_tab = 			(*Fonction à minimiser*)
	(* print_string "Dans fct_objectif\n" ;*)
	-1. *. float_tab.(0) *. float_tab.(1) *.float_tab.(2);; 		

let maj_vitesse part g = 					(*Calcul de la nouvelle vitesse à partir de la formule*)
	(* print_string "Dans maj_vitesse\n" ; *)
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in
	for i=0 to (d-1) do 
		part.vitesse.(i)<- w *. v.(i) +. c1 *. (m.(i) -. p.(i)) +. c2 *. (g.(i) -. p.(i)); 
	done;;

let contraintes part =						(*évalue liste de conditions de la forme f(variables)<=0*)
	(* print_string "Dans contraintes\n" ; *)
	let p = part.position in
	let x = p.(0) in
	(* print_string "\nx=";
	print_float x;
	print_string "\ny=";*)
	let y = p.(1) in
	(* print_float y;
	print_string "\nz=";*)
	let z = p.(2) in
	(* print_float z;
	print_string "\n";*)
	(										
		fonction_objectif p < fonction_objectif (part.meilleur) 
		&& x >= 0. 
		&& y >= 0. 
		&& z >= 0. 
		&& (x *. y +. 2. *. x *. z +. 2. *. y *. z) <= 1. 
	);;

let maj_position part = 					(*On a maj la vitesse, il faut déplacer la particule*)
	(* print_string "Dans maj_pos\n" ;*)
	for i=0 to (d-1) do 
		part.position.(i) <- part.position.(i) +. part.vitesse.(i);(*maj position*)
		if part.position.(i) > xmax then 	(*Les points qui sortent sont mis sur le bord et on annule leur vitesse*)
			begin
				part.position.(i) <- xmax; 
				part.vitesse.(i) <- 0.;
			end 	
		else 
			begin 
			if part.position.(i) < xmin then (*Les points qui sortent sont mis sur le bord et on annule leur vitesse*)
				begin
					part.position.(i) <- xmin; 
					part.vitesse.(i) <- 0.;
				end 
			end
	done;; 
	(* print_string "appel maj meilleur local\n" ;*)

let maj_meilleur_local part = 				(*Gestion des contraintes*)
	(* print_string "Dans maj meilleur local\n" ;*)
	if contraintes part then
	begin (*On remplace le meilleur local*)
		for i=0 to (d-1) do
			part.meilleur.(i) <- part.position.(i);
		done;
	end;;

let moyenne_float_array tab =				(*Fonction qui calcule la moyenne d'un tableau de float*) 
	(* print_string "Dans moyenne float arr\n" ;
	 *)let len = Array.length tab in
	let moy = ref(0.) in
	for i=0 to (len-1) do 
		moy := !moy +. tab.(i)
	done;
	!moy /. float_of_int(len);;

let maj_vitesse_moy particules =			(*Fonction qui calcule la norme moyenne de toutes les vitesses de toutes les particules*)
	(* print_string "Dans maj_vitesse moy\n" ;*)
	let v_moy = Array.make n 0. in
	for i=0 to (n-1) do
		v_moy.(i) <- moyenne_float_array particules.(i).vitesse;
	done;
	moyenne_float_array v_moy;;		

let maj_meilleur_global particules g = 		(*Compare les meilleurs locaux au global*)
	(* print_string "Dans maj_meilleur global\n" ;*)
	let nouveau_g = ref(g) in
	for i=0 to (n-1) do
		if (fonction_objectif (particules.(i).meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)

let affiche_tableau tab =					(*Affiche les éléments d'un tableau de flottants*)
	for i=0 to (Array.length tab -1) do
		print_float tab.(i);
		print_string "\n";
	done;;

let algo = fun _ -> 						(*Algo à lancer dans le main, print les meilleurs globaux et la valeur de la fonction objectif à chaque itération*)
	let particules = genere_particules n in 				
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g ->
		if (cmpt > iterMax || (var_vitesse < epsilon))  then g
		else 
			begin
				for i=0 to (n-1) do 
					maj_vitesse particules.(i) g; 
					maj_position particules.(i); 			 
					maj_meilleur_local particules.(i);
				done;
				let nvl_vitesse_moy = maj_vitesse_moy particules in	(*Pour tester si on stagne*)
				let nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy) in 
				let nvx_meilleur_global = maj_meilleur_global particules g in
				affiche_tableau nvx_meilleur_global;
				print_string "Valeur volume = ";
				print_float (fonction_objectif nvx_meilleur_global);
				print_string "\n";
				print_string "\n";
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy nvx_meilleur_global;
			end
	in 
	let var_vitesse = infini in
	let vitesse_moy = maj_vitesse_moy particules in
	let g = maj_meilleur_global particules (particules.(0).meilleur) in
	affiche_tableau g;
	print_string "Première Valeur volume = ";
	print_float (fonction_objectif g);
	print_string "\n";
	print_string "\n";
	particleswarm particules 0 var_vitesse vitesse_moy g;;	(*Checker valeurs initiales*)



algo();;