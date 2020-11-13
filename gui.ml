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

(* Creates a loop for the interface to keep running*)
(* And catches keyboard evens *)
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else print_char event.key; print_newline (); interactive ();;

 (*------- Graphics utility functions -------*)
 
let draw_path r = 
  Graphics.set_color path_color;
  let (a,b) = Graphics.current_point() in
  let pt0 = r.(0) in
  let x0 = int_of_float pt0.x in
  let y0 = int_of_float pt0.y in
  Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float pti.x in
  let yi = int_of_float pti.y in Graphics.lineto xi  yi
  done;
  Graphics.moveto a b;;

(* val draw_poly : Point array -> unit = <fun> *)
let draw_poly r =
  Graphics.set_color obstacles_color;
  let (a,b) = Graphics.current_point() in
  let pt0 = r.(0) in
  let x0 = int_of_float pt0.x in
  let y0 = int_of_float pt0.y in
  Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let pti = r.(i) in
  let xi = int_of_float pti.x in
  let yi = int_of_float pti.y in Graphics.lineto xi  yi
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
  Graphics.open_graph " 300x300";
  (* Plotting a *basically* smol triangle  *)

  

  draw_obstacles obstacles;
  draw_path path;

  (* Runing user interface loop *)
  interactive ();;


