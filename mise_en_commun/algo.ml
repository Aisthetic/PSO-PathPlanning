let n = 50;; 					(*Nombre de particules*) 
let nb = 5;;					(*Nombre de points*)
let d = 2 * nb;; 				(*Taille des tableaux dans particules*)
let iterMax = 50;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 10.;;					(*Vitesse max d'une particule*)
let infini = 1000000.			(*Infini*)
let p_obj = {Geometrie.x = 80.; y = 60.} (*Destination*) 
let obstacle = [[	{Geometrie.x=30.; y= 15.}; 
					{x=70.; y=50.}; 
					{x=34.; y= 52.}; 
					{x=30.; y= 15.}]; 
				[	{Geometrie.x=17.; y= 15.};
					{x=27.; y= 15.};
					{x=25.; y= 50.};
					{x=10.; y= 57.}; 
					{x=17.; y= 15.};]];;
let obstacle2 = [|[|{Geometrie.x=30.; y= 15.}; 
					{x=70.; y=50.}; 
					{x=34.; y= 52.}; 
					{x=30.; y= 15.}|]; 
				[|	{Geometrie.x=17.; y= 15.};
					{x=27.; y= 15.};
					{x=25.; y= 50.};
					{x=10.; y= 57.}; 
					{x=17.; y= 15.};|]|];;

(* Proeblèmes à resoudre : *)
(* coordonnées négatives *)
(* peu d'itérations *)
(* pas de solutions identiques en positif *)


type particule = Initialisation_avion.particule
(* trouver un moyen d'importer ca proprement plutot que de faire part.Ini... dans la suite*)

let affiche_tableau tab =					(*Affiche les éléments d'un tableau de flottants*)
	for i=0 to (Array.length tab -1) do
		print_float tab.(i);
		print_string "\n";
	done;;

let genere_particules n = 					(*Fonction qui génère un tableau de n particules, faite par Marc*)	
	Initialisation_avion.gen_swarm n nb p_obj xmax vmax obstacle;;

let float_array_to_point_array float_array =
	let len = Array.length float_array in
	let point_array = Array.make (len/2) {Geometrie.x=0.;y=0.} in
	let i = ref 0 in
	while !i < (len/2) do 
		point_array.(!i) <- {Geometrie.x=float_array.(2*(!i)); y=float_array.(2*(!i)+1)};
		i := !i + 1;
	done;
	point_array;;

let fonction_objectif float_array = 			(*Fonction à minimiser*)
	(* print_string "Dans fct_objectif\n" ;*)
	let len = Array.length float_array in
	let point_array = Array.make (len/2 -1) ({Geometrie.x=0.;y=0.}, {Geometrie.x=0.;y=0.}) in
	let i = ref 0 in
	while !i < (len/2 - 2) do 
		point_array.(!i) <- ({Geometrie.x=float_array.(2*(!i)); 
									y=float_array.(2*(!i)+1)},
								{	x=float_array.(2*(!i)+2); 
									y=float_array.(2*(!i)+3)});
		i := !i + 1;
	done;
	Geometrie.fonction_objectif point_array;; 		

let maj_vitesse part g = 					(*Calcul de la nouvelle vitesse à partir de la formule*)
	(* print_string "Dans maj_vitesse\n" ; *)
	let p = part.Initialisation_avion.position in
	let v = part.vitesse in
	let m = part.meilleur in
	let len = Array.length v in
	let new_v = ref 0. in
	for i=2 to (len - 3) do 
		new_v := w *. v.(i) +. c1 *. (m.(i) -. p.(i)) +. c2 *. (g.(i) -. p.(i));
		if  !new_v > vmax then part.vitesse.(i) <- vmax
		else part.vitesse.(i) <- !new_v;
	done;;



let part = {Initialisation_avion.position=[|0. ;0. ; 80.; 60.|] ;
			vitesse = [|0. ;0. ; 0.; 0.|];
			meilleur = [|0. ;0. ; 80.; 60.|]};;


(*probleème de parcours de toute la liste dans tous les cas : chercher l'aquivalent d'un break python dans une boucle for*)
let contraintes part =						(*évalue liste de conditions de la forme f(variables)<=0*)
	let test = ref true in
	for i=0 to (Array.length (part.Initialisation_avion.position) -1) do
		test :=(!test && (part.position.(i) >= 0.))
	done;(* 
	if !test then print_string "\ncoordonnees positives\n"; *)
	Printf.printf "\n trajectoire ok : %b\n" (Geometrie.trajectoire_ok (Initialisation_avion.array_to_point part.position) obstacle);
	(!test && (Geometrie.trajectoire_ok (Initialisation_avion.array_to_point part.position) obstacle))
	;;

affiche_tableau part.position;;
print_string "\n";;
Printf.printf "\nContraintes respectees :%b\n" (contraintes part);;

print_string "\n";;
 (* 

let maj_position part = 					(*On a maj la vitesse, il faut déplacer la particule*)
	(* print_string "Dans maj_pos\n" ;*)
	let len = Array.length part.Initialisation_avion.position in
	for i=2 to (len - 3) do 
		part.Initialisation_avion.position.(i) <- part.position.(i) +. part.vitesse.(i);(*maj position*)
	done;; 
	(* print_string "appel maj meilleur local\n" ;*)




let maj_meilleur_local part = 				(*Gestion des contraintes*)
	(* print_string "Dans maj meilleur local\n" ;*)
	if (contraintes part && ((fonction_objectif part.position) < (fonction_objectif part.meilleur))) then
	begin (*On remplace le meilleur local*)
		let len = Array.length part.Initialisation_avion.position in
		for i=2 to (len - 3) do 
			part.meilleur.(i) <- part.position.(i);
			print_float part.position.(i); 
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
		v_moy.(i) <- moyenne_float_array particules.(i).Initialisation_avion.vitesse;
	done;
	moyenne_float_array v_moy;;		

let maj_meilleur_global particules g = 		(*Compare les meilleurs locaux au global*)
	(* print_string "Dans maj_meilleur global\n" ;*)
	let nouveau_g = ref(g) in
	for i=0 to (n-1) do
		if (fonction_objectif (particules.(i).Initialisation_avion.meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)



let algo = fun _ -> 						(*Algo à lancer dans le main, print les meilleurs globaux et la valeur de la fonction objectif à chaque itération*)
	let particules = genere_particules n in 				
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g ->
		if (cmpt > iterMax (* || (var_vitesse < epsilon) *))  then begin print_string"\n\nglobal de fin";affiche_tableau g; g end
		else 
			begin
				for i=0 to (n-1) do (* 
					print_string "\nNombre d'itérations: \n";
					print_int cmpt; *)
					maj_vitesse particules.(i) g; 
					maj_position particules.(i); 			 
					maj_meilleur_local particules.(i);
					(* print_string "\nposition: \n";
					affiche_tableau particules.(i).position;
					print_string "\nmeilleur: \n";
					affiche_tableau particules.(i).meilleur;
					print_string "\nvitesse: \n";
					affiche_tableau particules.(i).vitesse;
					print_string "\n";
					print_string "\n"; *)
				done;
				let nvl_vitesse_moy = maj_vitesse_moy particules in	(*Pour tester si on stagne*)
				let nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy) in 
				let nvx_meilleur_global = maj_meilleur_global particules g in
				(* affiche_tableau nvx_meilleur_global;
				print_string "Longueur trajectoire = ";
				print_float (fonction_objectif nvx_meilleur_global);
				print_string "\nNombre d'itérations: ";
				print_int cmpt;
				print_string "\n";
				print_string "\n"; *)
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy nvx_meilleur_global;
			end
	in 
	let var_vitesse = infini in
	let vitesse_moy = maj_vitesse_moy particules in
	let g = maj_meilleur_global particules (particules.(0).meilleur) in
	affiche_tableau g;
	print_string "FONCTION OBJECTIF = ";
	print_float (fonction_objectif g);
	print_string "\n";
	print_string "\n";
	particleswarm particules 0 var_vitesse vitesse_moy g;;	(*Checker valeurs initiales*)




Gui.create obstacle2 (float_array_to_point_array (algo()));;