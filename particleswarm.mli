val n : int 					(*Nombre de particules*) 
val d : int 					(*Taille des tableaux dans particules*)
val iterMax : int 				(*Nombre d'itération max*)
val epsilon : float 			(*Précision de la variation de vitesse*)
val w : float 					(*Inertie*)
val c1 : float 					(*Indice de confiance cognitive*)
val c2 : float 					(*Indice de confiance sociale*)
val xmax : float 				(*Norme 1 max d'un point tournant autour du point de départ*)
val xmin : float 				(*Norme 1 min d'un point tournant autour du point de départ*)
val infini : float				(*Infini*)				

type particule = {position: float array; 
					vitesse: float array; 
					meilleur: float array} 			(*Initialisation*)


(*Fonctions qui doivent venir d'ailleurs*)
val genere_particules : int -> particule array 		(*Initialisation*)
(*val soustrait_points : point -> point -> point 	(*Geometry*)
val somme_points : point -> point -> point 			(*Geometry*)
val mult_point : point -> float -> point 			(*Geometry*)
val distance : point -> point -> float 				(*Geometry*) *)


(*Mes fonctions*)
(*Fonction qu'on cherche à MINIMISER*)
val fonction_objectif : float array -> float 

(*Calcul de la nouvelle vitesse *)
val maj_vitesse : particule -> float array -> unit 

(*Dis si contraintes sont respectées : domaines, et autres*)
val contraintes : particule -> bool
			
(*vitesse maj, déplace la particule et checke qu'elle est tjs bornée pour la maj locale*)
val maj_position : particule -> unit

(*maj le meilleur local de part après qu'elle soit à une nouvelle position*)
val maj_meilleur_local : particule -> unit		

(*calcule la moyenne d'un tableau de flottants*)
val moyenne_float_array : float array -> float
					
(*calcule la moyenne des normes des vitesses*)
val maj_vitesse_moy : particule array -> float 

(*compare les nouveaux meilleurs à l'ancien "g"*)
val maj_meilleur_global : particule array -> float array -> float array 

(*Affiche un tableau de flottants*)
val affiche_tableau : float array -> unit

(*Renvoie la meilleure position des points tournants*)
val algo : unit -> float array

			