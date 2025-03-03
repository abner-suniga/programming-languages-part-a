(* this is a comment *)

(* static typed enviroment *)

(* static and dynamic environments *)

(* variable bindings *)
val x = 34;
val y = 17;
val z = (x + 2) * y
val abs_of_z = if z < 0 then 0 - z else z;
val abs_of_y = abs y

(* syntax (how we write) -> semantics (type checking) -> evaluation (how we run) *)

(* REPL: read eval print loop *)

(* functions *)

fun pow (x : int, y: int) = 
  if y = 0 then 1 else x * pow(x, y-1)

val cube = pow(2,3)

(* tuples *)

fun div_mod (x : int, y : int) =
  (x div y, x mod y)


(* lists, all elements same type *)

val l = [10, 20, 30]
val range = 0::l

val empty = null []
val not_empty = null l

val head = hd l
val tail = tl l

fun sum_list (l: int list) =
  if null l then 0 else hd l + sum_list(tl l)

val sum = sum_list [0,1,2,3,4,5,6,7,8,9]

fun countdown (x: int) =
  if x = 0 then [] else x::countdown (x-1)

val count = countdown 7

fun append (l1 : int list, l2: int list) =
  if null l1 
  then
    if null l2
    then []
    else hd l2::append(l1, tl l2)
  else
    hd l1::append(tl l1, l2)

val lists = append([1,2,3],[4,5,6])

fun sum_pair_list (l: (int * int) list) =
  if null l
  then 0
  else (#1 (hd l)) + (#2 (hd l)) + sum_pair_list(tl l)

val list_pair = sum_pair_list([(1,2),(3,4),(5,6),(7,8),(9,0)])

(* let expressions and scope *)

(* boolean *)

val e = true andalso false
val ou = true orelse false

(* no mutations *)
(* no assignment statements *)
(* reference (alias) x copies doesn't matter if code is immutable *)
