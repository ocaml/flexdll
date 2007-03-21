let rec f x = if x = 0 then 1 else x * f (x - 1);;

print_int (f 10);;


