(* mod（剰余）の再定義 *)
let rec mymod a b = 
    if a < b
    then a
    else mymod (a - b) b;;

let a = 25 in
let b = 7 in
print_int (mymod a b);
print_newline();
print_int (mymod 100 b);
print_newline();
;;
