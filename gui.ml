(* This module contains the functions showing an user interface  
 * (we didn't decide what to show yet) 
.*)
(* source : https://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf *)


open Graphics;;
open Geometrie;;
(* En attendant que Geometry est debug par caro *)
type point = Geometrie.point

(* local variables *)
let path_color = Graphics.rgb 255 0 0;; (* red *)
let obstacles_color = Graphics.rgb 0 255 0;;
let initial_width = 600;; (* wight de la fen *)
let initial_heigth = 800;; (* height de la fen *)
let scale = ref 10.;; (* echelle utilisée *)
(*------- User Interface Utility Functions -------*)
 
let draw_path r = 
  (* On change de couleur pour tracer le chemin*)
  Graphics.set_color path_color;
  let (a,b) = Graphics.current_point() in
  (* On trace une ligne entre les points du tableau*)
  let pt0 = r.(0) in (* On se positionne au premier point du tableau *)
  let x0 = int_of_float (pt0.x *. !scale) in 
  let y0 = int_of_float (pt0.y *. !scale) in
  Graphics.moveto x0 y0; 
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float (pti.x *. !scale) in
  let yi = int_of_float (pti.y *. !scale) in Graphics.lineto xi  yi
  done;
  Graphics.moveto a b;; (* On se positionne au point sauvegarder *)

(* val draw_poly : Point array -> unit = <fun> *)
let draw_poly r =
  Graphics.set_color obstacles_color;
  let (a,b) = Graphics.current_point() in
  let pt0 = r.(0) in
  let x0 = int_of_float (pt0.x *. !scale) in
  let y0 = int_of_float (pt0.y *. !scale) in
  Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float (pti.x *. !scale) in
  let yi = int_of_float (pti.y *. !scale) in Graphics.lineto xi  yi
  done;
  Graphics.lineto x0 y0;
  Graphics.moveto a b;;

 
(* val draw_obstacles : Point array array-> unit = <fun> *)
let draw_obstacles r =
  for i = 0 to (Array.length r)-1 do
  let obsi = r.(i) in draw_poly obsi
  done

(* Creates a loop for the interface to keep running*)
(* And catches keyboard evens *)
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0 
  else print_char event.key; print_newline (); interactive ();;

(* creates a graphics window with it's main loop*)
let create () = 
  (* creating the graphics window*)
  Graphics.open_graph (Printf.sprintf " %dx%d" (initial_heigth) (initial_width));
  (* Runing user interface loop *)
  interactive ();;

(* return the maximum coordinates in the given arrays *)
let max_coordinates obstacles path =
  let rec obstacles_to_points obs points = 
    match obs with 
      [] -> points |
      t :: q -> obstacles_to_points q (Array.to_list t @ points)  in
  let obs_points = obstacles_to_points  (Array.to_list obstacles) [] in
  let rec max_coord_rec points max_coords = 
    match points with 
      [] ->  max_coords |
      t :: q -> max_coord_rec q {x = if max_coords.x < t.x then t.x else max_coords.x ; y = if max_coords.y < t.y then t.y else max_coords.y} in
  max_coord_rec ((Array.to_list path) @ obs_points) {x = 0.; y = 0.};;


let create obstacles path = 
  (* creating the graphics window*)
  Graphics.open_graph (Printf.sprintf " %dx%d" initial_heigth initial_width);
  (* calculating !scale*)
  let max_coords = max_coordinates obstacles path in
  let scale_x = float_of_int initial_width /. max_coords.x in 
  let scale_y = float_of_int initial_heigth /. max_coords.y in 
  scale := if scale_x > scale_y then scale_y else scale_x;

  (* Plotting figures*)
  draw_obstacles obstacles;
  draw_path path;

  (* Runing user interface loop *)
  interactive ();;

(*--------functions used to set data at any time of the app execution------*)
let set_data obstacles path = 
  Graphics.clear_graph ();
  draw_obstacles obstacles;
  draw_path path;