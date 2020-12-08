open Gui
open Geometrie
open Initialisation

(* data to test functions *)
let pt1 = {x = 20.; y = 20.};;
let pt2 = {x = 10.; y = 20.};;
let pt3 = {x = 20.; y = 10.};;
let polyg = [| pt1; pt2; pt3 |];;

let pt11 = {x = 13.; y = 0.};;
let pt21 = {x = 0.; y = 11.};;
let pt31 = {x = 13.; y = 13.};;
let polyg1 = [| pt11; pt21; pt31 |];;

let pt111 = {x = 0.; y = 0.};;
let pt211 = {x = 10.; y = 11.};;
let pt311 = {x = 13.; y = 20.};;

let path = [| pt111; pt211; pt311 |];;

let obstacles = [| polyg; polyg1 |];;

(* testing random obstacles generation *)
let obstacles_test = fun () ->
    let limits = [| 40. ; 40. |] in 
    let obstacles = Initialisation.gen_obstacles 3 2 limits 40. 6 in
    Utility.print_obstacles obstacles;;

(* gui automatic max pos calculation test *)
let max_coordinates_test = fun () ->
    let max_coords = Gui.max_coordinates obstacles path in 
    print_float max_coords.x;
    print_string "\n"; 
    print_float max_coords.y;;

let () =
    max_coordinates_test ();;