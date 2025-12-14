let rec make_adder x =
  let rec adder y = x + y in
  adder
in
let f = make_adder 10 in
print_int (f 20)
