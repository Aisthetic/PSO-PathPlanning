open Geometrie;;


type point= Geometrie.point;;

let p = {x=80.; y=60.};;
let obstacles = [ [{x=30.; y=30.}; {x=40.; y= 30.}; {x=40.; y= 40.}; {x=30.; y= 40.}; {x=30.; y=30.}] ];;
let path =  [{x=20.; y= 20.}; {x=20.; y=30.}] ;;
(* let swarm = Initialisation_adapte.gen_swarm 1 3 p obstacle;; *)

(* Initialisation_adapte.print_swarm swarm;; *)
if Geometrie.trajectoire_ok path obstacles then print_string "TRUE\n" else print_string "FALSE\n";;

(* let obs =Array.append [|Array.of_list obstacle1.(0)|] [|Array.of_list obstacle1.(1)|];; *)


let obstacles_ar = Array.of_list (List.map  Array.of_list obstacles);;
let path_ar = Array.of_list path;;
open Gui;;
Gui.create obstacles_ar path_ar;;

