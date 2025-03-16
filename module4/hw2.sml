(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str : string, strs: string list) =
  case strs of 
       []   => NONE
     | str'::strs' => 
        if same_string(str', str) then
          SOME(strs') 
        else 
          case all_except_option(str, strs') of
               NONE => NONE
             | SOME(sub_strs') => SOME(str'::sub_strs')

val all_except_option_test1 = all_except_option ("string", ["string"]) = SOME []
val all_except_option_test2 = all_except_option ("string", ["a", "b", "string", "c"]) = SOME ["a", "b", "c"]
val all_except_option_test3 = all_except_option ("STRING", ["a", "b", "string", "c"]) = NONE

fun get_substitutions1 (substitutions : string list list, s : string) =
  case substitutions of
       [] => []
     | sub::substitutions' => 
         let
           val all_except_s = all_except_option(s, sub)
         in
           case all_except_s of
             NONE => get_substitutions1(substitutions', s)
          |  SOME(sub_without_s) => sub_without_s @ get_substitutions1(substitutions', s)
         end
           
fun get_substitutions2_aux (substitutions : string list list, s : string, acc: string list) =
  case substitutions of
       [] => acc 
     | sub::substitutions' => 
         let
           val all_except_s = all_except_option(s, sub)
         in
           case all_except_s of
             NONE => get_substitutions2_aux(substitutions', s, acc)
          |  SOME(sub_without_s) => get_substitutions2_aux(substitutions', s, sub_without_s @ acc)
         end


fun get_substitutions2 (substitutions : string list list, s : string) =
  get_substitutions2_aux(substitutions, s, [])


val get_substitutions1_test1 = get_substitutions1([["foo"],["there"]], "foo")  
val get_substitutions1_test2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
val get_substitutions1_test3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

val get_substitutions2_test1 = get_substitutions2([["foo"],["there"]], "foo")  
val get_substitutions2_test2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
val get_substitutions2_test3 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")


fun gen_similar_name (_,[]) = []
  | gen_similar_name ({ first, middle, last }, n::names') = 
    ({ first=n, middle=middle, last=last })::gen_similar_name({ first=first ,middle=middle, last=last }, names')
  
fun similar_names (
  substitutions : string list list, 
  name: { first: string, middle: string, last: string}
) =
  case substitutions of
       [] => [name]
     | _  => name::gen_similar_name(name, get_substitutions1(substitutions, #first name)) 

val similar_names_test1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
           

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


(* Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)

fun card_color (Spades, _)    = Black
  | card_color (Clubs, _)     = Black
  | card_color (Diamonds, _)  = Red
  | card_color (Hearts, _)    = Red

val card_color_test1 = card_color (Clubs, Num 2) = Black

(* Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)

fun card_value (_, Num(number)) = number
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

val card_value_test1 = card_value (Clubs, Num 2) = 2


(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =. *)

fun remove_card ([], c, e) = raise e
  | remove_card ((c'::cs), c, e) = 
    if c' = c 
    then cs
    else c'::remove_card(cs, c, e)

val remove_card_test1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test2 = (remove_card ([], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true
val remove_card_test3 = remove_card ([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace), (Hearts, Ace)], (Clubs, Num 4), IllegalMove) = [(Hearts, Ace), (Hearts, Ace), (Hearts, Ace)]

(* Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures. *)

fun all_same_color [] = true
  | all_same_color [c] = true
  | all_same_color (c1::c2::cards') = (card_color(c1) = card_color(c2)) andalso all_same_color(c2::cards')

val all_same_color_test1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test2 = all_same_color [(Hearts, Ace), (Diamonds, Ace), (Diamonds, Num 7)] = true
val all_same_color_test3 = all_same_color [(Hearts, Ace), (Diamonds, Ace), (Spades, Num 7)] = false

(*  Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
requirement for this problem.) *)

fun sum_cards_tail ([], acc) = acc
  | sum_cards_tail (card::cards, acc) = sum_cards_tail(cards, acc + card_value(card))

fun sum_cards (cards) = sum_cards_tail (cards, 0)

val sum_cards_tail_test1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_tail_test2 = sum_cards [(Clubs, Ace),(Clubs, Num 2)] = 13

fun preliminary_score (held_cards, goal) =
  let 
    val sum = sum_cards(held_cards)
  in
    if sum > goal
    then 3 * (sum - goal)
    else (goal - sum)
  end

fun score (held_cards, goal) =
  let 
    val preliminary_score' = preliminary_score(held_cards, goal)
  in
    if all_same_color(held_cards)
    then  preliminary_score' div 2
    else preliminary_score'
  end


val score_test1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4


fun officiate_game_loop (_, [], goal, held_cards) = score(held_cards, goal)
  | officiate_game_loop (cards, (Discard discarded_card)::moves', goal, held_cards) = 
      officiate_game_loop(cards, moves', goal, remove_card(held_cards, discarded_card, IllegalMove))
  | officiate_game_loop ([], Draw::moves', goal, held_cards) = score(held_cards, goal)
  | officiate_game_loop (card::cards', Draw::moves', goal, held_cards) =
      if sum_cards(card::held_cards) > goal 
      then score(card::held_cards, goal)
      else officiate_game_loop(cards', moves', goal, card::held_cards)


fun officiate (cards, moves, goal) = officiate_game_loop(cards, moves, goal, [])

val officiate_test1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val officiate_test2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val officiate_test3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)




