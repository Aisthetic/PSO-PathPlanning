let d = 10;; (*Nombre de particules*) 
let iterMax = 100;; (*Nombre d'itération max*)
let epsilon = 0.1;; (*Précision de la variation de vitesse*)
let w = 0.72;; (*Inertie*)
let c1 = 1;; (*Indice de confiance cognitive*)
let c2 = 1;; (*Indice de confiance sociale*)


type particule = {position : float list; vitesse : float list; meilleur : float list}

let maj_vitesse part g =
	v = part.vitesse;
	p = part.position;
	part.vitesse = v *. w +. c1 *. (p -. x) +. c2 *. (g -. x); (*Créer les fonctions qui font ça sur les listes ou choisir tableaux*)

let maj_position part = ;(*On a maj la vitesse, il faut déplacer la particule et checker qu'lle est tojs bornée*)

let maj_meilleur_local part = ;

let algo = fun ->
	particules = genere_particules d; (*Fonction de Zakaria*)

	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy->
		if ((cmpt > iterMax) || (var_vitesse < epsilon)) then g
		else
			for i = 0 to (d-1) do (*part = ((x,y), vitesse, p)*)
				part = particules.(i);
				maj_vitesse part g; (*à écrire*)
				maj_position part; (*à écrire*)
				maj_meilleur_local part; (*à écrire*)
			g = maj_meilleur_global; (*à écrire*)
			nvl_vitesse_moy = maj_vitesse_moy particules; (*à écrire*)
			nvl_var_vitesse = abs_float (vitesse_moy - nvl_vitesse_moy); (*à écrire*)
			particuleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy (*à écrire*)
		done;
	in particleswarm particules 0 1;;