let rec print x = print_int x in
let rec sum k x =
  if x <= 0 then k 0
  else
    let rec cont r = k (r + x) in
    sum cont (x - 1)
in
sum print 10
