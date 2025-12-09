
let k = 42 in
let rec fib_alt n =
  if n <= 1 then n
  else 
    let x = fib_alt (n - 1) + fib_alt (n - 2) + k in
    x + x
in 
print_int (fib_alt 10)