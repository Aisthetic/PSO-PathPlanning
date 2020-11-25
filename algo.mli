val n : int 			(*Nombre de particules*) 
val d : int 			(*Nombre de points tournants*) 
val iterMax : int 		(*Nombre d'itération max*)
val epsilon : float 		(*Précision de la variation de vitesse*)
val w : float 			(*Inertie*)
val c1 : float 			(*Indice de confiance cognitive*)
val c2 : float 			(*Indice de confiance sociale*)
val xmax : float 		(*Norme 1 max d'un point tournant autour du point de départ*)

type point = {x:float; y:float} 					(*Geometry*)
type particule = {position: point array; 
					vitesse: point array; 
					meilleur: point array} 		(*Initialisation*)


(*Fonctions qui doivent venir d'ailleurs*)
val genere_particules : int -> particule array 		(*Initialisation*)
val soustrait_points : point -> point -> point 		(*Geometry*)
val somme_points : point -> point -> point 		(*Geometry*)
val mult_point : point -> float -> point 		(*Geometry*)
val fonction_objectif : point array -> float 		(*Geometry*)
val distance : point -> point -> float 			(*Geometry*)


(*Mes fonctions*)
(*Calcul de la nouvelle vitesse *)
val maj_vitesse : particule -> point array -> unit 
			
(*maj le meilleur local de part après qu'elle soit à une nouvelle position*)
val maj_meilleur_local : particule -> unit
			
(*vitesse maj, déplace la particule et checke qu'elle est tjs bornée*)
val maj_position_et_local : particule -> unit
			
(*compare les nouveaux meilleurs à l'ancien "g"*)
val maj_meilleur_global : particule array -> point array -> point array 
			
(*calcule la moyenne des normes des vitesses*)
val maj_vitesse_moy : particule array -> float 
			
(*Renvoie la meilleure position des points tournants*)
val algo : unit -> point array			
