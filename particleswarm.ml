let n = 10;; (*Nombre de particules*) 
let d = 2;; (*Nombre de points tournants*) 
let iterMax = 100;; (*Nombre d'itération max*)
let epsilon = 0.1;; (*Précision de la variation de vitesse*)
let w = 0.72;; (*Inertie*)
let c1 = 1;; (*Indice de confiance cognitive*)
let c2 = 1;; (*Indice de confiance sociale*)
let xmax = 100;; (*Norme 1 max d'un point tournant autour du point de départ*)

type particule = {position : point Array; vitesse : point Array; meilleur : point Array} (*point est un type def dans geometry.ml*)

let dans_repere pts_tournants = ;; (*bool de si pt tournant dans repère*)

let genere_particules n = ;; (*fonction qui génère un tableau de n particules, faite par Marc*)

let maj_vitesse part g = (*Calcul de la nouvelle vitesse à partir de la formule*)
	let p = part.position in
	let v = part.vitesse in
	let m = part.meilleur in (*voir si position ->x et meilleur ->p*)
	for i=0 to (d-1) do (*On suppose d fixé sinon, à passer en argument de la fonction*)
		let vx = v.(i).x *. w +. c1 *. (m.(i).x -. p.(i).x) +. c2 *. (g.(i).x -. p.(i).x) in
		let vy = v.(i).y *. w +. c1 *. (m.(i).y -. p.(i).y) +. c2 *. (g.(i).y -. p.(i).y) in
		part.vitesse.(i) <- point{x=vx; y=vy};; (*Voir si on peut *)

let maj_position_et_local part = (*On a maj la vitesse, il faut déplacer la particule et checker qu'lle est tojs bornée*)
	part.position <- somme_points (part.position) (part.vitesse);
	if dans_repere (part.position) (*à écrire*) then maj_meilleur_local part
	else for i=0 to (d-1) do 
		if part.(i).x > xmax then part.position.(i).x = xmax; part.vitesse.(i).x = 0; (*On place les points tournants sur le bord et on met la vitesse dans la direction qui fait sortir à 0*)
		if part.(i).y > xmax then part.position.(i).y = xmax; part.vitesse.(i).y = 0;
		maj_meilleur_local part;;


let maj_meilleur_local part = (*maj le meilleur local de part après qu'elle soit à une nouvelle position*)
	if fonction_objectif (part.position) < fonction_objectif (part.meilleur) then
	part.meilleur <- part.position;;

let maj_meilleur_global particules g = (*compare les nouveaux meilleurs à l'ancien "g"*)
	for i=0 to n do
		if fonction_objectif (particules.(i).meilleur) < fonction_objectif g then
			let g = particules.(i).meilleur in
	g;; (*point Array*)

let maj_vitesse_moy particules =
	let x = ref(0) in
	let y = ref(0) in
	for i=0 to (n-1) do
		let v = particules.(i).vitesse in 
		for j = 0 to (d-1) do
			x := v.(j).x + !x; 
			y := v.(j).y + !y;
			
	;;


let algo = fun ->
	particules = genere_particules n; (*à écrire*)

	let rec particleswarm = fun particules cmpt var_vitesse vitesse_moy->
		if ((cmpt > iterMax) || (var_vitesse < epsilon)) then g
		else
			for i = 0 to (n-1) do 
				part = particules.(i);
				maj_vitesse part; (*maj vitesse*)
				maj_position_et_local part; (* maj position et meilleur local*)
			g = maj_meilleur_global particules g; (*g = point Array*)
			nvl_vitesse_moy = maj_vitesse_moy particules; (*à écrire*)
			nvl_var_vitesse = abs_float (vitesse_moy - nvl_vitesse_moy); (*à écrire*)
			particuleswarm particules (cmpt+1) nvl_var_vitesse nvl_vitesse_moy (*à écrire*)
		done;
	in particleswarm particules 0 1;;