open Gui
open Geometrie
open Initialisation_obstacles

(* data to test functions *)
let pt1 = {x = 20.; y = 20.};;
let pt2 = {x = 10.; y = 20.};;
let pt3 = {x = 20.; y = 10.};;
let polyg = [| pt1; pt2; pt3 |];;
let obstacle0 = { sommets = polyg; vitesse = (5.,3.) };;

let pt11 = {x = 13.; y = 0.};;
let pt21 = {x = 0.; y = 11.};;
let pt31 = {x = 13.; y = 13.};;
let polyg1 = [| pt11; pt21; pt31 |];;
let obstacle1 = { sommets = polyg1; vitesse = (10.,7.) };;

let pt111 = {x = 0.; y = 0.};;
let pt211 = {x = 10.; y = 11.};;
let pt311 = {x = 13.; y = 20.};;

let path = [| pt111; pt211; pt311 |];;

let polygs = [| polyg; polyg1 |];;
let obstacles = [| obstacle0; obstacle1 |];;
(* testing random obstacles generation *)
(* let obstacles_test = fun () ->
    let limits = [| 40. ; 40. |] in 
    let obstacles = Initialisation.gen_obstacles 3 2 limits 40. 6 in
    Utility_print.print_obstacles obstacles;; *)

(* gui automatic max pos calculation test *)
let max_coordinates_test = fun () ->
    let max_coords = Gui.max_coordinates polygs path in 
    print_float max_coords.x;
    print_string "\n"; 
    print_float max_coords.y;;

(* test de deplacemement d'obstacle*)
let deplacemement_obstacle_test = fun () ->
    let poly_dep = Geometrie.deplacer_obstacle 100. 100. polyg in 
    Utility_print.print_obstacle {sommets = poly_dep; vitesse = (0., 0.)};;

(* test de la generation d'obstacles dynamiques *)
let gen_etats_test = fun () ->
    let etats = Initialisation_obstacles.gen_etats obstacles 1. 3 in 
    Utility_print.print_etats etats;;

let () =
    gen_etats_test ();;