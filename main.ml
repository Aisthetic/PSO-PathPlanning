open Graphics;;
Graphics.open_graph " 200x200";;
let f x = x + 1;;
for i = 0 to 200 do
  plot i (f i)
done;;