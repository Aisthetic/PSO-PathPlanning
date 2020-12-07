type particule = {
  position : float array;
  vitesse : float array;
  best_pos : float array;
}
val gen_point : int -> float array -> float array
val generation : int -> int -> float array -> float array
val gen_swarm : int -> int -> int -> float array -> particule array
val print_array : float -> unit
val print_particule : particule -> unit
val print_swarm : particule array -> unit