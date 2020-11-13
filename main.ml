
open Gui

let pt1 = {x = 200.; y = 200.};;
let pt2 = {x = 100.; y = 200.};;
let pt3 = {x = 200.; y = 100.};;
let polyg = [| pt1; pt2; pt3 |];;

let pt11 = {x = 133.; y = 0.};;
let pt21 = {x = 0.; y = 111.};;
let pt31 = {x = 133.; y = 133.};;
let polyg1 = [| pt11; pt21; pt31 |];;

let pt111 = {x = 0.; y = 0.};;
let pt211 = {x = 100.; y = 111.};;
let pt311 = {x = 133.; y = 200.};;

let path = [| pt111; pt211; pt311 |];;

let obstacles = [| polyg; polyg1 |];;

Gui.create obstacles path;;