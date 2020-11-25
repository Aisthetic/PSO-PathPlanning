(* This module contains the functions showing an user interface  
 * (we didn't decide what to show yet) 
.*)
(* source : https://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf *)


open Graphics;;
(* open Geometry;; *)
(* En attendant que Geometry est debug par caro *)
type point = {x : float; y : float}

let path_color = Graphics.rgb 255 0 0;; (* red *)
let obstacles_color = Graphics.rgb 0 255 0;;
let initial_width = 600;; (* wight de la fen *)
let initial_heigth = 800;; (* height de la fen *)
let scale = 10.;; (* echelle utilisée *)
(* Creates a loop for the interface to keep running*)
(* And catches keyboard evens *)
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0 
  (* refresh () ; *)
  else print_char event.key; print_newline (); interactive ();;

(* actualisation des données - si nécessaire *)
(* let refresh () = () *)

 (*------- User Interface Utility Functions -------*)
 
let draw_path r = 
  (* On change de couleur pour tracer le chemin*)
  Graphics.set_color path_color;
  let (a,b) = Graphics.current_point() in
  (* On trace une ligne entre les points du tableau*)
  let pt0 = r.(0) in (* On se positionne au premier point du tableau*)
  let x0 = int_of_float (pt0.x *. scale) in 
  let y0 = int_of_float (pt0.y *. scale) in
  Graphics.moveto x0 y0; 
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float (pti.x *. scale) in
  let yi = int_of_float (pti.y *. scale) in Graphics.lineto xi  yi
  done;
  Graphics.moveto a b;; (* On se positionne au point sauvegardé*)

(* val draw_poly : Point array -> unit = <fun> *)
let draw_poly r =
  Graphics.set_color obstacles_color;
  let (a,b) = Graphics.current_point() in
  let pt0 = r.(0) in
  let x0 = int_of_float (pt0.x *. scale) in
  let y0 = int_of_float (pt0.y *. scale) in
  Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float (pti.x *. scale) in
  let yi = int_of_float (pti.y *. scale) in Graphics.lineto xi  yi
  done;
  Graphics.lineto x0 y0;
  Graphics.moveto a b;;

 
(* val draw_obstacles : Point array array-> unit = <fun> *)
let draw_obstacles r =
  for i = 0 to (Array.length r)-1 do
  let obsi = r.(i) in draw_poly obsi
  done;;

  (* creates a graphics window with it's main loop*)
let create obstacles path = 
  (* creating the graphics window*)
  Graphics.open_graph (Printf.sprintf " %dx%d" initial_heigth initial_width);
  (* Plotting a *basically* smol triangle  *)

  draw_obstacles obstacles;
  draw_path path;

  (* Runing user interface loop *)
  interactive ();;


