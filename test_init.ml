open Initialisation_adapte;;
open Geometrie;;


type point= Geometrie.point;;

let p = {x=80.; y=60.};;
let obstacle = [[{x=30.; y= 15.}; {x=70.; y=50.}; {x=34.; y= 52.}; {x=30.; y= 15.}]; [{x=10.; y= 15.};{x=25.; y= 15.};{x=25.; y= 50.};{x=10.; y= 50.}; {x=10.; y= 15.};]];;

let swarm = Initialisation_adapte.gen_swarm 1 3 p obstacle;;

Initialisation_adapte.print_swarm swarm;;
if (Geometrie.trajectoire_ok (Array.to_list swarm.(0).position) obstacle) then print_string "TRUE\n" else print_string "FALSE\n";;

let obstacle1 = Array.of_list obstacle;;
let obs =Array.append [|Array.of_list obstacle1.(0)|] [|Array.of_list obstacle1.(1)|];;



open Gui;;
Gui.create obs swarm.(0).position;;
