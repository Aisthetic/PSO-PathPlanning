open Geometrie
open Init

let n = 20;; 					(*Nombre de particules*) 
let nb = 2;;					(*Nombre de points*)
let iterMax = 200;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 20.;;				(*Vitesse max d'une particule*)
let infini = 100000.			(*Infini*)
let p_obj = { x = 80.; y = 60.} (*Destination*) 
let punition = 2.;;				(*Coefficient qui multiplie la distance parconrue dans un obstacle*) 
let obstacle = [[	{x=30.; y= 15.}; 
					{x=70.; y= 50.}; 
					{x=34.; y= 52.}; 
					{x=30.; y= 15.}]; 
				[	{x=17.; y= 15.};
					{x=27.; y= 15.};
					{x=25.; y= 50.};
					{x=10.; y= 57.}; 
					{x=17.; y= 15.};]];;
let obstacle2 = [|[|{x=30.; y= 15.}; 
					{x=70.; y= 50.}; 
					{x=34.; y= 52.}; 
					{x=30.; y= 15.}|]; 
				[|	{x=17.; y= 15.};
					{x=27.; y= 15.};
					{x=25.; y= 50.};
					{x=10.; y= 57.}; 
					{x=17.; y= 15.};|]|];;




(*UTILITY *)
(*Affiche les éléments d'un tableau de flottants*)
let affiche_tableau tab =					
	for i=0 to (Array.length tab -1) do
		print_float tab.(i);
		print_string "\n";
	done;;

let print_point = fun p ->
	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ") ";;

let print_point_array tab = Array.iter print_point tab;;


(*Convertit tableau de float en tableau de points*)
let float_array_to_point_array float_array =
	let len = Array.length float_array in
	let point_array = Array.make (len/2) {Geometrie.x=0.;y=0.} in
	let i = ref 0 in
	while !i < (len/2) do 
		point_array.(!i) <- {Geometrie.x=float_array.(2*(!i)); y=float_array.(2*(!i)+1)};
		i := !i + 1;
	done;
	point_array;;


(* permet l'affichage de l'ensemble des particules *)
let print_swarm = fun swarm ->
	Array.iter print_particule swarm;;
	print_string "\n";;

(*Affiche une liste de point*)
let rec print_point_list pt_list = match pt_list with
	|[] -> ()
	|p::q -> 	print_string "("; print_float p.x; print_string ";"; print_float p.y; print_string ")\n ";
				print_point_list q;;








(*ALGO*)

(* génère n particules de nb points tournants, position = [|0;0; etc ;pobjx; pobjy|], obstacle une point list list*)
let genere_particules n nb p_obj xmax vmax obstacle = 					
	gen_swarm n nb p_obj xmax vmax obstacle;;

(* print_swarm (genere_particules n nb p_obj xmax vmax obstacle);; *)



(*Fonction à minimiser: longueur de la trajectoire pénalisée*)
let fonction_objectif float_array obstacle = (*Attention, les obstacles n'ont pas le droit de se superposer*) 
	let traj = Geometrie.construit_traj (array_to_point float_array) obstacle in
	print_string "\nTrajectoire est \n";print_traj traj;print_string "\n\n";
	let rec calcule traj valeur = match traj with 
		|[] -> valeur
		|[p1] -> valeur
		|(p1,inout1)::(p2,inout2)::q when inout1 != inout2 -> (*Dans ce cas p1 = p2 on est sur une frontière*)
				print_string "valeur: \n";print_float valeur;print_string "\n";
				calcule ((p2,inout2)::q) valeur
		|(p1,inout1)::(p2,inout2)::q when inout1 = Geometrie.In -> (*On est dans un obstacle*)
				print_string "valeur: \n";print_float valeur;print_string "\n";
				calcule ((p2,inout2)::q) (punition *. (distance p1 p2) +. valeur)
		|(p1,inout1)::(p2,inout2)::q when inout1 = Geometrie.Out -> (*On est hors des obstacle*)
				print_string "valeur: \n";print_float valeur;print_string "\n";
				calcule ((p2,inout2)::q) ((distance p1 p2) +. valeur)
		|_ -> failwith "devrait pas arriver"
	in calcule traj 0.


(* let traj = [|   0. ;20.;
			   50. ;20.  |];;

let a = {x=10.;y=10.};;
let b = {x=30.;y=10.};;
let c = {x=30.;y=30.};;
let d = {x=10.;y=30.};;

let ob = [[a;b;c;d;a]];;
*)
(* let traj = [|  
0.;
0.;
31.6026228111;
21.2845040971;
36.8497540827;
52.2075916682;
80.;
60.
|];; (*74*) *)

let gen_traj = fun nb xmax -> (*Génère une trajectoire aléatoire*)
	let rec rec_gen = fun trajectoire increment ->
		if increment = nb then Array.append trajectoire [|xmax;xmax|]
		else
			rec_gen (Array.append trajectoire [|Random.float xmax|]) (increment+1)
	in 
		let t= (rec_gen [|0.;0.|] 0) in
		print_string "\n points tournants";affiche_tableau t;print_string "\n";
		Printf.printf "distance = %f \n"(fonction_objectif t obstacle)
	;;	

gen_traj 2 80.;;
(* let traj = [|  
0.;
0.;
60.;
60.;
80.;
60.
|];; (*74*) 
Printf.printf "distance_finale = %f \n"(fonction_objectif traj obstacle);;  *)


(*Calcul de la nouvelle vitesse à partir de la formule*)
let maj_vitesse part g w c1 c2 = 					
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in
	let len = Array.length v in
	let new_v = ref 0. in
	for i=2 to (len - 3) do 
		new_v := w *. v.(i) +. c1 *. (m.(i) -. p.(i)) +. c2 *. (g.(i) -. p.(i));
		if  !new_v > vmax then part.vitesse.(i) <- vmax 
		else
			if  !new_v < -1. *. vmax then part.vitesse.(i) <- -1. *. vmax
			else part.vitesse.(i) <- !new_v;
	done;;


(* maj_vitesse part [|0.; 0.; 70.; 10.; 80.; 60.|] w c1 c2;;
print_string "vitesse \n";;
affiche_tableau part.vitesse;;
print_string "\n position \n";;
affiche_tableau part.position;; *)


(* jtm<<<caro *)
(*On a maj la vitesse, il faut déplacer la particule*)
let maj_position part = 					
	let len = Array.length part.position in
	for i=2 to (len - 3) do 
		part.position.(i) <- part.position.(i) +. part.vitesse.(i);(*maj position*)
	done;; 


(* maj_position part;;
print_string "\n position apres bouger \n";;
affiche_tableau part.position;;
 *)




(*maj_meilleur_local*)
let maj_meilleur_local part obstacle = 			
	if ((fonction_objectif part.position obstacle) < (fonction_objectif part.meilleur obstacle)) then
	begin (*On remplace le meilleur local*)
		let len = Array.length part.position in
		for i=2 to (len - 3) do 
			part.meilleur.(i) <- part.position.(i);
		done;
	end;;

(* print_string "\nmeilleur local avant\n";;
affiche_tableau part.meilleur;;
maj_meilleur_local part obstacle;;
print_string "\nmeilleur local\n";;
affiche_tableau part.meilleur;; *)


(*Fonction qui calcule la moyenne d'un tableau de float*)
let moyenne_float_array tab =				 
	let len = Array.length tab in
	let moy = ref(0.) in
	for i=0 to (len-1) do 
		moy := !moy +. tab.(i)
	done;
	!moy /. float_of_int(len);;

(*Fonction qui calcule la norme moyenne de toutes les vitesses de toutes les particules*)
let maj_vitesse_moy particules =			
	let v_moy = Array.make n 0. in
	for i=0 to (n-1) do
		v_moy.(i) <- moyenne_float_array particules.(i).vitesse;
	done;
	moyenne_float_array v_moy;;		

(*Compare les meilleurs locaux au global*)
let maj_meilleur_global particules g obstacle = 		
	let nouveau_g = ref 0 in
	for i=0 to (n-1) do
		if (fonction_objectif (particules.(i).meilleur) obstacle < fonction_objectif g obstacle) then
			nouveau_g := i;
	done;
	particules.(!nouveau_g).meilleur;; (*point Array*)



(*Algo à lancer dans le main, print les meilleurs globaux et la valeur de la fonction objectif à chaque itération*)
let algo = fun _ -> 	(*mettre les args*)					
	let particules = genere_particules n nb p_obj xmax vmax obstacle in 				
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g ->
		if (cmpt > iterMax (* || (var_vitesse < epsilon) *))  
		then 
			begin 
				print_string "\nmeilleure trajectoire trouvee : \n";
				affiche_tableau g;
				print_string "\nFONCTION OBJECTIF = ";
				print_float (fonction_objectif g obstacle);
				print_string "\n";
				g;
			end
		else 
			begin
				for i=0 to (n-1) do  
					(* print_string "\nNombre d'itérations: \n";
					print_int cmpt;  *)
					maj_vitesse particules.(i) g w c1 c2; 
					maj_position particules.(i); 			 
					maj_meilleur_local particules.(i) obstacle;
					(* print_string "\nposition: \n";
					affiche_tableau particules.(i).position;
					print_string "\nvitesse: \n";
					affiche_tableau particules.(i).vitesse;
					print_string "\nmeilleur: \n";
					affiche_tableau particules.(i).meilleur;
					print_string "\n";
					print_string "\n";  *)
				done;
				let nvl_vitesse_moy = maj_vitesse_moy particules in	(*Pour tester si on stagne*)
				let nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy) in 
				let nvx_meilleur_global = maj_meilleur_global particules g obstacle in
				Printf.printf "\nmeilleur global\n";
				affiche_tableau nvx_meilleur_global;
				print_string "\nLongueur trajectoire = ";
				print_float (fonction_objectif nvx_meilleur_global obstacle);
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy nvx_meilleur_global;
			end
	in 
	let var_vitesse = infini in
	let vitesse_moy = maj_vitesse_moy particules in
	let g = maj_meilleur_global particules (particules.(0).meilleur) obstacle in
	print_string "\n";
	particleswarm particules 0 var_vitesse vitesse_moy g;;	(*Checker valeurs initiales*)


(* Gui.create obstacle2 (float_array_to_point_array (algo()));; *)


(* Printf.printf "\ntraj ok %b \n" (trajectoire_ok [{x=0.;y=0.};{x=13.9;y=8.23};{x=80.;y=60.}] obstacle);; *)