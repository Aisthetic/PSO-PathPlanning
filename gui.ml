(* This module contains the functions showing an user interface  
 * (we didn't decide what to show yet) 
.*)
(* source : https://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf *)


open Graphics;;
(* open Geometry;; *)
(* En attendant que Geometr est debug par caro *)
type point = {x : float; y : float}

(* Creates a loop for the interface to keep running*)
(* And catches keyboard evens *)
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else print_char event.key; print_newline (); interactive ();;

 (*------- Graphics utility functions -------*)
 
(* val draw_poly : Point array -> unit = <fun> *)
let draw_poly r =
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

(* creating the graphics window*)
Graphics.open_graph " 300x300";;
(* let f x = x + 1;;
for i = 0 to 200 do
  plot i (f i) *)
let pt1 = {x = 0.; y = 0.}
let pt2 = {x = 133.; y = 0.}
let pt3 = {x = 0.; y = 111.}
let polyg = [| pt1; pt2; pt3 |];;
draw_poly polyg ;;

(* Runing user interface loop *)
interactive ();;


