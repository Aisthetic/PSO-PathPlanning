open Geometrie
open Initialisation_avion

(*let n = 50;; 					(*Nombre de particules*) 
let nb = 3;;					(*Nombre de points*)
let iterMax = 1000;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 20.;;				(*Vitesse max d'une particule*)
let infini = 1000000.			(*Infini*)
let p_obj = { x = 80.; y = 60.} (*Destination*) *)
(* let obstacle = [[	{x=30.; y= 15.}; 
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
 *)
let a = {x=15. ; y = 0. };;
let b = {x=15. ; y = 15.};;
let c = {x=25. ; y = 10.};;
let d = {x=30. ; y = 20.};;
let e = {x=40. ; y = 20.};;
let f = {x=40. ; y = 25.};;
let g = {x=55. ; y = 35.};;
let h = {x=55. ; y = 50.};;
let i = {x=70. ; y = 50.};;
let j = {x=70. ; y = 0.};;
let k = {x= 0. ; y = 25.};;
let l = {x=30. ; y = 25.};;
let m = {x=45. ; y = 45.};;
let n = {x=45. ; y = 60.};;
let o = {x=0. ; y = 60.};;

let o1 = [a;c;d;e;f;g;h;i;j;a];;
let o2 = [k;l;m;n;o;k];;
let obst = [o1;o2];;
let obst_b = [| [|a;c;d;e;f;g;h;i;j;a|] ; [|k;l;m;n;o;k|] |];;

let n = 5;; 					(*Nombre de particules*) 
let nb = 3;;					(*Nombre de points*)
let iterMax = 1000;;	 			(*Nombre d'itération max*)
let epsilon = 0.001;; 			(*Précision de la variation de vitesse*)
let w = 0.72;; 					(*Inertie*)
let c1 = 1.;; 					(*Indice de confiance cognitive*)
let c2 = 1.;; 					(*Indice de confiance sociale*)
let xmax = 100.;; 				(*Norme 1 max d'un point tournant autour du point de départ*)
let xmin = 0.;; 				(*Norme 1 min d'un point tournant autour du point de départ*)
let vmax = 20.;;				(*Vitesse max d'une particule*)
let infini = 1000000.;;			(*Infini*)
let p_obj = { x = 80.; y = 60.};;

let print_seg = fun s ->
	let a,b = s in
	let xa = a.x in
	let ya = a.y in
	let xb = b.x in
	let yb = b.y in
	Printf.printf "[ (%f,%f) , (%f,%f) ]\n" xa ya xb yb
;;




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


let test_chq = fun t obst ->
	let tab = float_array_to_point_array t in
	for i=0 to (Array.length tab - 2) do
		let p1 = tab.(i) in
		let p2 = tab.(i+1) in
		let s = (p1,p2) in
		let rec aux1 = fun o ->
			match o with 
				|[] -> print_string "Fin obstacle\n\n"
				|[p] -> print_string "Fin obstacle\n\n"
				|h1::h2::q -> print_seg (h1,h2); Printf.printf "Croise ce segment %b\n" (croise_segment (h1,h2) s); aux1 (h2::q)
		in
		let rec aux = fun obst j ->
			match obst with 
				|[] -> print_string "Fin segment\n\n"
				|h::q -> let tst = croise_obstacle s h in aux1 h; Printf.printf "Croise_obstacle %d : %b \n\n\n" j tst; aux q (j+1)
		in aux obst 1;
	done;;

(*ALGO*)

(* génère n particules de nb points tournants, position = [|0;0; etc ;pobjx; pobjy|], obstacle une point list list*)
let genere_particules n nb p_obj xmax vmax obstacle = 					
	gen_swarm n nb p_obj xmax vmax obstacle;;


(*Fonction à minimiser: longueur de la trajectoire*)
let fonction_objectif float_array = 			
	let len = Array.length float_array in
	let i = ref 0 in
	let dist = ref 0. in
	while !i < (len - 3) do
		let xa = float_array.(!i) in 
		let ya = float_array.(!i+1) in 
		let xb = float_array.(!i+2) in 
		let yb = float_array.(!i+3) in 
		let d = distance {x=xa;y=ya} {x=xb;y=yb} in
		dist := !dist +. d;
		i := !i+2;
	done;
	!dist;;

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

(*On a maj la vitesse, il faut déplacer la particule*)
let maj_position part = 					
	let len = Array.length part.position in
	for i=2 to (len - 3) do 
		part.position.(i) <- part.position.(i) +. part.vitesse.(i);(*maj position*)
	done;; 


 let rec print_lst = fun lst ->
 	match lst with
 		|[] -> Printf.printf "Fin de liste\n\n"
 		|t::q -> Printf.printf "(%f,%f)\n" t.x t.y; print_lst q
 ;;


(*évalue liste de conditions de la forme f(variables)<=0*)
let contraintes part obstacle =						
	let test = ref true in
	let i = ref 0 in
	let len = (Array.length part.position) in
	while (!test && (!i < len)) do  
		test := (part.position.(!i) >= 0.);
		incr i;
	done;
	let t_ok = (test_traj part.position obstacle) in
	(!test && t_ok);;

(*Gestion des contraintes*)
let maj_meilleur_local part obstacle = 	
	let cnt = contraintes part obstacle in
	let best =((fonction_objectif part.position) < (fonction_objectif part.meilleur)) in
	if (cnt && best) then
		begin (*On remplace le meilleur local*)
			let len = Array.length part.position in
			for i=2 to (len - 3) do 
				part.meilleur.(i) <- part.position.(i);
			done;
			print_string "Je change \n\n";
		end

;;

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
let maj_meilleur_global particules g = 		
	let nouveau_g = ref(g) in
	for i=0 to (n-1) do
		if (fonction_objectif (particules.(i).meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)



(*Algo à lancer dans le main, print les meilleurs globaux et la valeur de la fonction objectif à chaque itération*)
let algo = fun obst -> 						
	let particules = genere_particules n nb p_obj xmax vmax obst in 				
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g ->
		if (cmpt > iterMax (* || (var_vitesse < epsilon) *))  
		then 
			begin 
				Printf.printf "\nMon meilleur global respecte: %b\n" (test_traj g obst);
				(*test_chq g obst;*)
				print_string "\nmeilleure trajectoire trouvee : \n";
				affiche_tableau g;
				print_string "\nFONCTION OBJECTIF = ";
				print_float (fonction_objectif g);
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
					maj_meilleur_local particules.(i) obst;
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
				let nvx_meilleur_global = maj_meilleur_global particules g in
				
				(*Printf.printf "\nmeilleur global\n";
				affiche_tableau nvx_meilleur_global;
				print_string "\nLongueur trajectoire = ";
				print_float (fonction_objectif nvx_meilleur_global); *)
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy nvx_meilleur_global;
			end
	in 
	let var_vitesse = infini in
	let vitesse_moy = maj_vitesse_moy particules in
	let g = maj_meilleur_global particules (particules.(0).meilleur) in
	print_string "\n";
	particleswarm particules 0 var_vitesse vitesse_moy g;;	(*Checker valeurs initiales*)


Gui.create obst_b (float_array_to_point_array (algo(obst)));;



(* Printf.printf "\ntraj ok %b \n" (trajectoire_ok [{x=0.;y=0.};{x=13.9;y=8.23};{x=80.;y=60.}] obstacle);; *)