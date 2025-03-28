fun compose_opt fopt gopt a' = 
  let   
    val g_result = gopt a'
  in
    case g_result of
         NONE => NONE
       | SOME v => fopt (v)
  end

fun div10_by x = 
  if x = 0
  then NONE
  else SOME (10 div x)

fun non_negative x =
  if x < 0 
  then NONE
  else SOME x

val compose_opt_test_1 = compose_opt div10_by non_negative 0
val compose_opt_test_2 = compose_opt div10_by non_negative 1
val compose_opt_test_3 = compose_opt div10_by non_negative ~1


(* 

1 = 1 
2 = 2*1 = 2
3 = 

*)

