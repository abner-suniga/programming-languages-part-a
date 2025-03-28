exception NoAnswer

(* 1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)

infix |> (* tells the parser |> is a function that appears between its two arguments *)
fun x |> f = f x

val only_capitals = List.filter(
  fn str => String.sub(str, 0) |> Char.isUpper
) 

val only_capitals_test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test2 = only_capitals ["A","b","C"] = ["A","C"]


(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)

val longest_string1 = List.foldl(
  fn (curr_str, longest_str) => 
    if String.size(curr_str) > String.size(longest_str)
    then curr_str
    else longest_str
) ""

val longest_string1_test1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test2 = longest_string1 ["Ax","bc","C"] = "Ax"

(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size. *)

val longest_string2 = List.foldl(
  fn (curr_str, longest_str) => 
    if String.size(curr_str) >= String.size(longest_str)
    then curr_str
    else longest_str
) ""

val longest_string2_test1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test2 = longest_string2 ["Ax","bc","C"] = "bc"

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper. *)

val longest_string_helper = 
  fn compare => List.foldl(
    fn (curr_str, longest_str) => 
      if compare(String.size(curr_str), String.size(longest_str))
      then curr_str
      else longest_str) "" 

(* *)

val longest_string3 = longest_string_helper (fn (i, j) => i > j)
val longest_string4 = longest_string_helper (fn (i, j) => i >= j)

val longest_string3_test1  = longest_string3 ["A","bc","C"] = "bc"
val longest_string4_test1  = longest_string4 ["A","B","C"] = "C"

(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2. *)


val longest_capitalized = fn lst => 
  lst |> only_capitals |> longest_string1
val longest_capitalized_test1 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test2 = longest_capitalized ["A","bc","CC", "BB"] = "CC"

(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. (Browse the module documentation to find the most useful functions.) *)

val rev_string = fn str => 
  str |> String.explode |> List.rev |> String.implode 

val rev_string_test_1  = rev_string "abc" = "cba"
val rev_string_test_2  = rev_string "a" = "a"
val rev_string_test_3  = rev_string "ab" = "ba"

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy. *)

fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs) =
      case f x of
          SOME v => v
        | NONE => first_answer f xs;

val first_answer_test_1  = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

(* 8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME []. *)


fun all_answers f [] = SOME []
  | all_answers f (x::xs) =
    case f x of
         NONE => NONE
       | SOME v => 
           case all_answers f xs of
                NONE => NONE
              | SOME vs => SOME(v @ vs)

val all_answers_test1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val all_answers_test2 = all_answers (fn x => if x = 1 orelse x = 4 then SOME [x] else NONE) [1,4] = SOME [1,4]
val all_answers_test3 = all_answers (fn x => if x = 1 orelse x = 4 then SOME [x] else NONE) [1,4,5] = NONE
          
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(*  (This problem uses the pattern datatype but is not really about pattern-matching.) A function g has
been provided to you.
(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains. *)

fun count_wildcards p = g (fn x => 1) (fn x => 0) p

val count_wildcards_test_1 = count_wildcards (Wildcard) = 1
val count_wildcards_test_2 = count_wildcards (TupleP([Wildcard, Wildcard])) = 2
val count_wildcards_test_3 = count_wildcards (TupleP([Wildcard, Variable("a"), Wildcard])) = 2

(* (b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. (Use String.size. We care only about variable names; the
constructor names are not relevant.) *)

fun count_wild_and_variable_lengths p = g (fn x => 1) (fn x => String.size(x)) p
val count_wild_and_variable_lengths = count_wild_and_variable_lengths (Variable("a")) = 1

(* (c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant. *)

fun count_some_var (s, p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p

val count_some_var_test_1 = count_some_var ("x", TupleP([Variable("x"), Wildcard])) = 1
val count_some_var_test_2 = count_some_var ("y", TupleP([Variable("x"), Wildcard])) = 0


(* 10. Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
names are not relevant. Hints: The sample solution uses two helper functions. The first takes a
pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses
@ is useful in one case. The second takes a list of strings and decides if it has repeats. List.exists may
be useful. Sample solution is 15 lines. These are hints: We are not requiring foldl and List.exists
here, but they make it easier. *)

fun extract_variables p = 
	case p of
	    Wildcard          => []
	  | Variable x        => [x] 
	  | TupleP ps         => List.foldl (fn (p,i) => (extract_variables p) @ i) [] ps
	  | ConstructorP(_,p) => extract_variables p
	  | _                 => []

val extract_variables_test_1 = extract_variables (TupleP([Variable("x"), Wildcard, Variable("y")])) = ["y", "x"]

fun check_string_repeats strs = 
  case strs of
       [] => false
     | (s::strs') => 
         (List.exists (fn x => x = s) strs') orelse (check_string_repeats strs')

fun check_pat p = 
  p 
  |> extract_variables
  |> check_string_repeats 
  |> not

val check_pat_test1 = check_pat (Variable("x")) = true
val check_pat_test2 = check_pat (TupleP([Variable("x"), Wildcard, Variable("y")])) = true
val check_pat_test3 = check_pat (TupleP([Variable("x"), Wildcard, Variable("x")])) = false
val check_pat_test4 = check_pat (TupleP([Variable("b"), TupleP([Variable("a"), Wildcard, Variable("y")]), Variable("y")])) = false
val check_pat_test5 = check_pat (TupleP([Variable("b"), TupleP([Variable("a"), Wildcard, Variable("y")]), Variable("z")])) = true

(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines. Remember to look above for the
rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier. *)

fun match (v, p)  = 
  case (v, p) of 
       (_, Wildcard) => SOME []
     | (v', Variable variable_name) => SOME [(variable_name , v')]
     | (Unit, UnitP) => SOME []
     | (Const v_integer, ConstP integer) => 
         if integer = v_integer
         then SOME []
         else NONE
     | (Tuple vs, TupleP ps) => 
         if List.length(vs) = List.length(ps)
         then all_answers match (ListPair.zip (vs, ps))
         else NONE
     | (Constructor (s2, v'), ConstructorP (s1, p')) => 
         if s1 = s2 
         then match(v', p')
         else NONE
     | _ => NONE

val match_test1 = match (Const(1), UnitP) = NONE

val match_test2 = match (Tuple([Const 12, Unit, Constructor ("Card", Unit)]),
Variable ("my_var")) = SOME [("my_var", Tuple([Const 12, Unit, Constructor ("Card", Unit)]))]

val match_test3 = match (Tuple([Const 12, Unit, Constructor ("Card", Unit)]),
TupleP([Variable "my_var"])) = NONE

val match_test4 = match (Tuple([Const 12, Unit, Constructor ("Card", Unit)]),
TupleP([Variable "twelve", Wildcard, ConstructorP("Card", UnitP)])) = SOME [("twelve",Const 12)]

fun first_match v ps =
  SOME (first_answer (fn (p) => match (v, p)) ps)
  handle NoAnswer => NONE

val first_match_test_1  = first_match Unit [UnitP] = SOME []
val first_match_test_2  = first_match Unit [Variable "my_var", UnitP] = SOME [("my_var", Unit)]
val first_match_test_3  = first_match Unit [ConstP 10, TupleP([UnitP])] = NONE


