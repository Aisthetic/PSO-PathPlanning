let n = 10;; (*Nombre de particules*) 
let d = 2;; (*Nombre de points tournants*) 
let iterMax = 100;; (*Nombre d'itération max*)
let epsilon = 0.1;; (*Précision de la variation de vitesse*)
let w = 0.72;; (*Inertie*)
let c1 = 1.;; (*Indice de confiance cognitive*)
let c2 = 1.;; (*Indice de confiance sociale*)
let xmax = 100.;; (*Norme 1 max d'un point tournant autour du point de départ*)

type point = {x:float; y:float};; (*Existe déjà dans geometry*)
type particule = {position: point array; vitesse: point array; meilleur: point array};; (*point est un type def dans geometry.ml*)

let genere_particules n = [| {position=[|{x=0.;y=0.}|]; vitesse = [|{x=0.;y=0.}|]; meilleur= [|{x=0.;y=0.}|]} |];; (*fonction qui génère un tableau de n particules, faite par Marc*)
let fonction_objectif traj = 0.;; (*Juste pour pas avoir d'erreur, prendre fonction Caro*)
let soustrait_points pt1 pt2 = {x=pt1.x -. pt2.x; y=pt1.y -. pt2.y};;
let somme_points pt1 pt2 = {x=pt1.x +. pt2.x; y=pt1.y +. pt2.y};;
let mult_point pt coeff = {x=coeff *. pt.x; y=coeff *. pt.y};;
let distance pt1 pt2 = 0.;; (*Fonction def dans Geometry*)

let maj_vitesse part g = (*Calcul de la nouvelle vitesse à partir de la formule*)
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in (*voir si position ->x et meilleur ->p*)
	let nvlv = ref({x=0.; y=0.}) in
	for i=0 to (d-1) do (*On suppose d fixé sinon, à passer en argument de la fonction*)
		nvlv := somme_points (somme_points (mult_point v.(i) w) (mult_point (soustrait_points m.(i) p.(i)) c1)) (mult_point (soustrait_points g.(i) p.(i)) c2);
		part.vitesse.(i)<- (!nvlv); done;;

let maj_meilleur_local part = (*maj le meilleur local de part après qu'elle soit à une nouvelle position*)
	if fonction_objectif (part.position) < fonction_objectif (part.meilleur) then
	(*part.meilleur <- part.position*)
	for i=0 to (d-1) do
		part.meilleur.(i) <- part.position.(i);
	done;;

let maj_position_et_local part = (*On a maj la vitesse, il faut déplacer la particule et checker qu'lle est tojs bornée*)
	for i=0 to (d-1) do (*maj position*)
		part.position.(i) <- somme_points (part.position.(i)) (part.vitesse.(i));
	done; 
	for i=0 to (d-1) do (*check si hors-bornes*)
		if part.position.(i).x > xmax then part.position.(i) <- {x=xmax;y=part.position.(i).y}; part.vitesse.(i) <- {x=0.; y=part.vitesse.(i).y}; (*On place les points tournants sur le bord et on met la vitesse dans la direction qui fait sortir à 0*)
		if part.position.(i).y > xmax then part.position.(i) <- {x=part.position.(i).x;y=xmax}; part.vitesse.(i) <- {x=part.vitesse.(i).x; y=0.}; done;
	maj_meilleur_local part;;


let maj_meilleur_global particules g = (*compare les nouveaux meilleurs à l'ancien "g"*)
	let nouveau_g = ref(g) in
	for i=0 to n do
		if (fonction_objectif (particules.(i).meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)

let maj_vitesse_moy particules =
	let norme = ref(Array.make d 0.) in
	let v = ref(Array.make d {x=0.;y=0.}) in 
	let m_v = ref(0.) in
	let m = ref(0.) in
	for i=0 to (n-1) do 
		v := particules.(i).vitesse; 
		norme := Array.map (fun pt -> distance pt {x=0.; y=0.} ) !v;
		for j=0 to (d-1) do
			m_v := !m_v +. !norme.(j);
		done;
		m_v := !m_v /. (float_of_int d);
		m := !m +. !m_v;
	done;
	!m /. float_of_int(n);;


let algo = fun _ ->
	let particules = genere_particules n in (*à écrire*)
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy g->
		if ((cmpt > iterMax) || (var_vitesse < epsilon)) then g
		else 
			begin
				for i=0 to (n-1) do 
					let part = particules.(i) in
					maj_vitesse part g; (*maj vitesse*)
					maj_position_et_local part; 
				done;(* maj position et meilleur local*)
				let nvl_vitesse_moy = maj_vitesse_moy particules in (*renvoie un float*)
				let nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy) in (*à écrire*)
				particleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy (maj_meilleur_global particules g); (*à écrire*)
			end
	in particleswarm particules 0 1. 3. particules.(0).meilleur;;