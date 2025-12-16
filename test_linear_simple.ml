let rec print x = print_int x in
let rec f k x = k (x + 1) in
f print 10
