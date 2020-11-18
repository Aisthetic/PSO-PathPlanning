let n = 10;; (*Nombre de particules*) 
let d = 2;; (*Nombre de points tournants*) 
let iterMax = 100;; (*Nombre d'itération max*)
let epsilon = 0.1;; (*Précision de la variation de vitesse*)
let w = 0.72;; (*Inertie*)
let c1 = 1.;; (*Indice de confiance cognitive*)
let c2 = 1.;; (*Indice de confiance sociale*)
let xmax = 100;; (*Norme 1 max d'un point tournant autour du point de départ*)

type point = {x:float; y:float};; (*Existe déjà dans geometry*)
type particule = {position: point array; vitesse: point array; meilleur: point array};; (*point est un type def dans geometry.ml*)

let genere_particules n = ();; (*fonction qui génère un tableau de n particules, faite par Marc*)

let soustrait_points pt1 pt2 = {x=pt1.x -. pt2.x; y=pt1.y -. pt2.y};;
let somme_points pt1 pt2 = {x=pt1.x +. pt2.x; y=pt1.y +. pt2.y};;
let mult_point pt coeff = {x=coeff *. pt.x; y=coeff *. pt.y};;

let maj_vitesse part g = (*Calcul de la nouvelle vitesse à partir de la formule*)
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in (*voir si position ->x et meilleur ->p*)
	let v = ref({x=0.; y=0.}) in
	for i=0 to (d-1) do (*On suppose d fixé sinon, à passer en argument de la fonction*)
		v := somme_points (somme_points (mult_point v.(i) w) (mult_point (soustrait_points m.(i) p.(i)) c1)) (mult_point (soustrait_points g.(i) p.(i)) c2);
		part.vitesse.(i)<- (!v); done;;

let maj_position_et_local part = (*On a maj la vitesse, il faut déplacer la particule et checker qu'lle est tojs bornée*)
	part.position <- somme_points (part.position) (part.vitesse);
	for i=0 to (d-1) do 
		if part.(i).x > xmax then part.position.(i).x = xmax; part.vitesse.(i).x = 0; (*On place les points tournants sur le bord et on met la vitesse dans la direction qui fait sortir à 0*)
		if part.(i).y > xmax then part.position.(i).y = xmax; part.vitesse.(i).y = 0; done;
	maj_meilleur_local part;;


let maj_meilleur_local part = (*maj le meilleur local de part après qu'elle soit à une nouvelle position*)
	if fonction_objectif (part.position) < fonction_objectif (part.meilleur) then
	part.meilleur <- part.position;;

let fonction_objectif traj = 0.;; (*Juste pour pas avoir d'erreur, prendre fonction Caro*)
let maj_meilleur_global particules g = (*compare les nouveaux meilleurs à l'ancien "g"*)
	let nouveau_g = ref(g) in
	for i=0 to n do
		if (fonction_objectif (particules.(i).meilleur) < fonction_objectif g) then
			nouveau_g := particules.(i).meilleur;
	done;
	!nouveau_g;; (*point Array*)

let distance pt1 pt2 = 0.;; (*Fonction def dans Geometry*)
let maj_vitesse_moy particules =
	let n = ref(Array.make d 0) in
	let v = ref(Array.make d 0) in 
	let m_v = ref(0.) in
	let m = ref(0.) in
	for i=0 to (n-1) do 
		v := particules.(i).vitesse; 
		n := Array.iter (fun pt -> distance pt {x=0.; y=0.} ) !v;
		for j=0 to (d-1) do
			m_v := !m_v +. !n.(j);
		done;
		m_v := !m_v /. d;
		m := !m +. !m_v;
	done;
	!m /. n;;


let algo = fun _ ->
	particules = genere_particules n; (*à écrire*)
	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy->
		if ((cmpt > iterMax) || (var_vitesse < epsilon)) then g
		else
			for i = 0 to (n-1) do 
				part = particules.(i);
				maj_vitesse part; (*maj vitesse*)
				maj_position_et_local part; done;(* maj position et meilleur local*)
			g = maj_meilleur_global particules g; (*g = point Array*)
			nvl_vitesse_moy = maj_vitesse_moy particules; (*renvoie un float*)
			nvl_var_vitesse = abs_float (vitesse_moy -. nvl_vitesse_moy); (*à écrire*)
			particuleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy; (*à écrire*)
	in particleswarm particules 0 1;;