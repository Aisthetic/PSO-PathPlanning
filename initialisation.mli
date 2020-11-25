type point = { x : float; y : float; }
type particule = {
  position : point array;
  vitesse : point;
  best_pos : point array;
}
val p_init : point
val p_final : point
val nb_pt : int
val d : int
val gen_point : point -> point
val generation : int -> point array
val gen_swarm : int -> int -> particule array
val print_point : point -> unit
val print_array : point array -> unit
val print_particule : particule -> unit
val print_swarm : particule array -> unit
