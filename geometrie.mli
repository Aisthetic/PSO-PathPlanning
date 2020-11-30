type point

(* Donne la différence acceptable entre deux flottants *)
val epsilon : float

(* Fonction qui donne la distance entre deux points *)
val distance : point -> point -> float

val fonction_objectif : point array -> float 

val mult_point : point -> float -> point 

val soustrait_points : point -> point -> point 

val somme_points : point -> point -> point

(* Fonction qui donne les paramètres d'une droite connaissant deux de ses points *)
val equation_droite : point -> point -> float * float

(* Fonction qui donne les paramètres des droites formant le polygone *)
val equation_obstacle : point list -> (float * float) list

(* Fonction qui donne les segmenst composant le polygone *)
val segmente_obstacle : point list -> (point * point) list

(* Fonction qui donne le point d'intersection de deux droites *)
val intersection_droites : point * point -> point * point -> point

(* Fonction qui indique si un point se situe dans un segment *)
val pt_dans_seg : point -> point * point -> bool

(* Fonction qui indique si deux segments se croisent *)
val croise_segment : point * point -> point * point -> bool

(* Fonction qui indique si un segment traverse un obstacle *)
val croise_obstacle : point * point -> point list -> bool

(* Fonction qui indique si un segment de la trajectoire traverse un des obstacles *)
val trajectoire_ok : point list -> (point list) list -> bool