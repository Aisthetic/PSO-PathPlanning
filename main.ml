open Graphics;;
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else print_char event.key; print_newline (); interactive ();;
Graphics.open_graph " 200x200";;
let f x = x + 1;;
for i = 0 to 200 do
  plot i (f i)
done;;
interactive ();;
