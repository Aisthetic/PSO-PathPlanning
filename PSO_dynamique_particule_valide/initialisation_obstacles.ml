(* This module contains the functions needed to generate a dynamic but predictible obstacles*)
open Geometrie

let pas_temps = 0.5;; (* le pas à considérer entre chaque 2 états *)

(* ce type regroupe les données nécessaire pour un obstacle dynamique à savoir position et vitesse*)
type obstacle = Geometrie.obstacle

(* un etat donne la position de tout les obstacles à un instant t*)
type etat = Geometrie.etat


let calculer_nouveau_obstacle = fun t obstacle ->
    let (dx, dy) = match obstacle.vitesse with vx,vy -> vx*.t, vy*.t in  
    {sommets = Geometrie.deplacer_obstacle dx dy obstacle.sommets ; vitesse =  obstacle.vitesse }


(* var gen_etats = obstacle arrray -> float -> int -> etat array  *)
(* cette fonctions va générer le nombre d'états nombre_etats avec entre chacun  *)
(* une durée pas_temps à partir des obstacles passés en paramètre *)
let gen_etats = fun obstacles pas_temps nombre_etats  ->
    let rec gen_etats_rec = fun etats nb_etats ->
        if nb_etats = nombre_etats then etats
		else
            let temps = pas_temps*. float_of_int nb_etats in 
			gen_etats_rec (Array.append etats [| { t = temps ; obstacles = Array.map (calculer_nouveau_obstacle temps) obstacles } |]) (nb_etats+1) in 
    gen_etats_rec [||] 0;;

    