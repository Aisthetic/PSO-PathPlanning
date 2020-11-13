type point = {x : float; y : float}

val draw_path : point array -> unit
val draw_poly : point array -> unit
val draw_obstacles : point array array-> unit
val create : point array array ->  point array -> unit