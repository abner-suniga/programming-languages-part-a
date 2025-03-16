(* Write a function 
alternate : int list -> int
alternate : int list -> int that takes a list of numbers and adds them with alternating sign. For example 
alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2
alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2. *)
fun alternate (l : int list) =
  let 
    fun alternate_aux (l: int list, pos: int) =
      if null(l)
      then 0
      else 
        if pos mod 2 = 1
        then hd(l) + alternate_aux(tl(l), pos+1)
        else ~(hd(l)) + alternate_aux(tl(l), pos+1)
  in
    alternate_aux(l, 1)
  end

val alternate_test1 = alternate([1,2,3,4])
val alternate_test2 = alternate([1,2,3])

(* Write a function 
min_max : int list -> int * int
min_max : int list -> int * int that takes a non-empty list of numbers, and returns a pair 
(min, max)
(min, max) of the minimum and maximum of the numbers in the list. *)
fun min_max (l : int list) =
  if null(l)
  then NONE
  else 
    let 
      val curr = min_max(tl(l))
      val min = if isSome(curr) 
        then SOME(#1 (valOf(curr)))
        else NONE
      val max = if isSome(curr) 
        then SOME(#2 (valOf(curr)))
        else NONE
      val new_min = 
        if isSome(min) andalso valOf(min) < hd(l)
        then valOf(min)
        else hd(l)
      val new_max = 
        if isSome(max) andalso valOf(max) > hd(l)
        then valOf(max)
        else hd(l)
    in
      SOME((new_min, new_max))
    end

val min_max_test1 = min_max([45,2,34,12,41,25,54,34])


fun alternate2 (l : int list) =
  if null(l)
  then 0
  else hd(l) - alternate2(l) 

val alternate2_test1 = alternate([1,2,3,4])
val alternate2_test2 = alternate([1,2,3])

(* Write a function 
cumsum : int list -> int list
cumsum : int list -> int list that takes a list of numbers and returns a list of the partial sums of those numbers. For example 
cumsum [1,4,20] = [1,5,25]
cumsum [1,4,20] = [1,5,25]. *)
fun cumsum (l : int list) =
  let
    fun cumsum1 (l : int list, sum : int) =
      if null(l)
      then []
      else (sum + hd(l))::cumsum1(tl(l), sum + hd(l))
  in
    cumsum1(l, 0)
  end

val cumsum_test1 = cumsum([1,4,20])

(* Write a function 
greeting : string option -> string
greeting : string option -> string that given a string  option 
SOME
SOME name returns the string 
"Hello there, ...!"
"Hello there, ...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is 
NONE
NONE then replace the dots with 
"you"
"you". *)
fun greeting (name : string option) =
  case name of
    NONE => "Hello there, you!"
  | SOME name => "Hello there, " ^ name ^ "!"

val greeting_test_1 = greeting(SOME "Abner")
val greeting_test_2 = greeting(NONE)


(* Write a function 
repeat : int list * int list -> int list
repeat : int list * int list -> int list that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: 
repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]. *)

fun repeatn (n, x) =
  if n = 0
  then [] 
  else x :: repeatn(n-1, x)

val repeatn_test1 = repeatn (3, 9) = [9,9,9]

fun append (xs, ys) =
  case xs of 
    [] => ys
  | x::xs => x :: append(xs, ys)

fun repeat (alist : int list, rlist : int list) =
  case alist of
    [] => []
  | e::alist' => append(repeatn(hd rlist, e), repeat(alist', tl rlist))

val repeat_test1 = repeat([1,2,3], [4,0,3]) 
