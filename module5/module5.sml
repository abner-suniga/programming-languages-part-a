(* 

Functional programming

1. Avoid mutation
2. Function as values

Functional language -> The functional programming is the conventional way to do it 

*)

(* First class functions *)

fun double x = 2 * x
fun incr x = x + 1
val sixteen = double(incr 7)

(* I love how type inference works in SML, functional paramaters are generic by
* default, no need to add ugly language features like generics <T>
*
* You can type them if you want, but the compiler does all the type
* checking for. *)

(* Anonymous functions *)

fun map (_, []) = []
  | map (f, item::rest) = f(item)::map(f, rest)

val tripled = map(fn x => 3 * x, [1,2,3])

(* Closures -> Keep the old environment
* Lexical scope (static) -> environment where the function is defined 
* lexical scope is fundamental for high order functions *)

(* Exceptions work like dynamic scopes *)

(* Function composition *)
(* Pipelines *)

(* Currying *)

(* Partial application *)

(* References *)

(* List.map, List.filter *)

(* Closure are kind like oop objects *)

(* high order programming for languages that don't suport closures *)

(* C only function pointers, it doesn't bring the environment *)

(* Language ahead of their time, garbage collection, generis, HIGH ORDER
* FUNCTIONs, type inference *)

(* Better for parrallel programming *)
