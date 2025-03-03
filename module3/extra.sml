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



