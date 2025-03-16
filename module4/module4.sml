(* Records 
* + type inferences 
* *)

val x = {
  bar = 1+2,
  baz = (false, 9)
}

val me = {
  id = 13144102,
  name = "Abner"
}
val my_name = #name me

(* Tuples and Records are similar "Each of" types 
* Records are accessed by name and tuples by position 
* We can think of functions in many languages as a hybrid 
* the arguments are passed by position and the params 
* accessed by name *)

(* Tuples Synctactic Sugar, Tuples are Records *)
val my_tuple1 = {
  1 = "a",
  2 = "b"
}

val my_tuple2 = ("a", "b")

(* Datatype Bindings ("One of") and Patterns *)
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype exp = 
  Constant of int
| Negate   of exp
| Add      of exp * exp
| Multiply of exp * exp

fun max_costant (e: exp) =
  case e of
    Constant c => c
  | Negate e1 => max_costant(e1)
  | Add (e1, e2) => Int.max(max_costant e1,max_costant e2)
  | Multiply (e1, e2) => Int.max(max_costant e1,max_costant e2)

      
val max_costant_test1 = max_costant(Add (Constant 19, Negate (Constant 4))) = 19

(* Type alias *)
type card = rank * suit

(* Polymorphic datatypes *)
(* Example a -> type for internal nodes, b -> type for leaves *)
(*datatype (a, b) tree =
  Node of a * (a, b) tree * (a, b) tree
| Leaf of b*)

(* Val-binding patterns *)
(* val p = e and fun f p = e *)
(* Every function in ML takes 1 argument, it makes easier to pipe an output
* of a function to input of another *)


fun sum_triple (x, y, z) =
  x + y + z

fun full_name {first=x, middle=y, last=z} =
  x ^ " " ^ y ^ " " ^ z

(* Homework 2, do not use #, do not write explicit types *)

fun non_decreasing [] = true
  | non_decreasing [x] = true
  | non_decreasing (x1::x2::xs') = x1 <= x2 andalso non_decreasing (x2::xs')

fun non_decreasing2 xs =
  case xs of
       [] => true
    |  [_] => true
    |  x1::x2::xs' => x1 <= x2 andalso non_decreasing (x2::xs')

val non_decreasing_test1 = non_decreasing2([1,2,3])
val non_decreasing_test2 = non_decreasing2([1,2,0])

(* Exceptions *)

(* Tail recursion *)




