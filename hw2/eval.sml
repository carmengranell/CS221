structure Eval : sig

  val isV  : Desugared.term -> bool
  val isNV : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list


  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared

  fun isNV D.Zero = true
            | isNV(D.Succ t1) = (case isNV(t1)
                              of true => true
                              | false => false)                      
            | isNV(_) = false

  fun isV (D.Pair(t1, t2)) = if (isV t1)
                                then isV t2
                                else false
    | isV(t) = isNV t
              
		 
  fun step(D.Zero) = NONE
    | step (D.Add(D.Zero, t2)) = SOME(t2) 
    | step (D.Succ(t1)) = (case (step t1)
                            of SOME t1' => SOME(D.Succ(t1'))
                            | NONE => NONE
                          )
    | step (D.Add(D.Succ(v1), t2)) = if (isV v1)
                                        then SOME(D.Add(v1, D.Succ(t2)))
                                        else (case (step v1)
                                               of SOME v1' => SOME(D.Add(D.Succ(v1'), t2))
                                               | NONE => NONE)
    | step(D.Add(t1, t2)) = (case (step t1)
                              of SOME t1' => SOME(D.Add(t1', t2))
                              | NONE => NONE)
    | step(D.Subtract(D.Zero, v2)) = if (isV v2)
                                       then SOME(D.Zero)
                                        else (case (step v2)
                                              of SOME v2' => SOME(D.Subtract(D.Zero, v2'))
                                              | NONE => NONE) 
    | step(D.Subtract(v1, D.Zero)) = if (isV v1)
                                       then SOME(v1)
                                        else (case (step v1)
                                                of SOME v1' => SOME(D.Subtract(v1', D.Zero))
                                                | NONE => NONE) 
    | step(D.Subtract((D.Succ v1), (D.Succ v2))) =  (case (isV v1)
                                                    of true => (case (isV v2)
                                                          of true => SOME(D.Subtract(v1, v2))
                                                          | false => (case (step v2)
                                                                    of SOME v2' => SOME(D.Subtract(D.Succ(v1), D.Succ(v2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step v1)
                                                           of SOME v1' => SOME(D.Subtract(D.Succ(v1'), D.Succ(v2)))
                                                           | NONE => NONE))  
    | step(D.Subtract(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(D.Subtract(t1, t2'))
                                      | NONE => NONE)
                              else (case (step t1)
                                      of SOME t1' => SOME(D.Subtract(t1', t2))
                                      | NONE => NONE)  

    | step(D.Less(D.Zero, D.Zero)) = SOME(D.Zero)   
    | step(D.Less(D.Zero, D.Succ(v2))) = if (isV v2)
                                            then SOME(D.Succ(D.Zero))
                                            else NONE  
    | step(D.Less(v1, D.Zero)) = if (isV v1)
                                    then SOME(D.Zero)
                                    else NONE                                                                                                                    
    | step(D.Less(D.Succ(v1), D.Succ(v2))) = (case (isV v1)
                                                    of true => (case (isV v2)
                                                          of true => SOME(D.Less(v1, v2))
                                                          | false => (case (step v2)
                                                                    of SOME v2' => SOME(D.Less(D.Succ(v1), D.Succ(v2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step v1)
                                                           of SOME v1' => SOME(D.Less(D.Succ(v1'), D.Succ(v2)))
                                                           | NONE => NONE))
    | step(D.Less(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(D.Less(t1, t2'))
                                      | NONE => NONE)
                              else (case (step t1)
                                      of SOME t1' => SOME(D.Less(t1', t2))
                                      | NONE => NONE)
    | step(D.Eq(D.Zero, D.Zero)) = SOME(D.Succ(D.Zero))
    | step(D.Eq(D.Zero, D.Succ(v2))) = if (isV v2)
                                       then SOME(D.Zero)
                                       else (case (step v2)
                                              of SOME v2' => SOME(D.Eq(D.Zero, D.Succ(v2')))
                                              | NONE => NONE
                                            )
    | step(D.Eq(D.Succ(v1), D.Zero)) = if (isV v1)
                                       then SOME(D.Zero)
                                       else (case (step v1)
                                              of SOME v1' => SOME(D.Eq(D.Succ(v1'), D.Zero))
                                              | NONE => NONE
                                            )
    | step(D.Eq(D.Succ(v1), D.Succ(v2))) = (case (isV v1)
                                                    of true => (case (isV v2)
                                                          of true => SOME(D.Eq(v1, v2))
                                                          | false => (case (step v2)
                                                                    of SOME v2' => SOME(D.Eq(D.Succ(v1), D.Succ(v2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step v1)
                                                           of SOME v1' => SOME(D.Eq(D.Succ(v1'), D.Succ(v2)))
                                                           | NONE => NONE))
    | step(D.Eq(D.Pair(v1, v2), D.Pair(v3, v4))) = if (isV v1)
                                                   then (if (isV v2)
                                                         then (if (isV v3)
                                                               then (if (isV v4)
                                                                     then SOME(D.Cond (D.Eq(v1, v3), D.Eq(v2, v4), D.Zero))
                                                                     else (case (step v4)
                                                                            of SOME v4' => SOME(D.Eq(D.Pair(v1, v2), D.Pair(v3, v4')))
                                                                            | NONE => NONE
                                                                          )
                                                                    ) 
                                                               else (case (step v3)
                                                                      of SOME v3' => SOME(D.Eq(D.Pair(v1, v2), D.Pair(v3', v4)))
                                                                      | NONE => NONE
                                                                    )
                                                              )
                                                         else (case (step v2)
                                                                of SOME v2' => SOME(D.Eq(D.Pair(v1, v2'), D.Pair(v3, v4)))
                                                                | NONE => NONE
                                                              )
                                                        )
                                                   else (case (step v1)
                                                          of SOME v1' => SOME(D.Eq(D.Pair(v1', v2), D.Pair(v3, v4)))
                                                          | NONE => NONE
                                                        )
    
    | step(D.Eq(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(D.Eq(t1, t2'))
                                      | NONE => NONE)
                              else (case (step t1)
                                      of SOME t1' => SOME(D.Eq(t1', t2))
                                      | NONE => NONE)
    | step(D.Cond(D.Succ(D.Zero), t2, _)) = SOME(t2)
    | step(D.Cond(D.Zero, _, t3)) = SOME(t3)
    | step(D.Cond(t1, t2, t3)) = (case (step t1)
                                      of SOME t1' => SOME(D.Cond(t1', t2, t3))
                                      | NONE => NONE)
    | step(D.Pair(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(D.Pair(t1, t2'))
                                      | NONE => NONE)
                              else (case (step t1)
                                      of SOME t1' => SOME(D.Pair(t1', t2))
                                      | NONE => NONE)
    
    | step(D.First(D.Pair(v1, v2))) = if (isV v1)
                                      then (if (isV v2)
                                            then SOME(v1)
                                            else (case (step v2)
                                                  of SOME v2' => SOME(D.First(D.Pair(v1, v2')))
                                                  | NONE => NONE
                                                 )
                                           )
                                      else (case (step v1)
                                            of SOME v1' => SOME(D.First(D.Pair(v1', v2)))
                                            | NONE => NONE
                                           )
    | step(D.Second(D.Pair(v1, v2))) = if (isV v1)
                                      then (if (isV v2)
                                            then SOME(v2)
                                            else (case (step v2)
                                                  of SOME v2' => SOME(D.Second(D.Pair(v1, v2')))
                                                  | NONE => NONE
                                                 )
                                           )
                                      else (case (step v1)
                                            of SOME v1' => SOME(D.Second(D.Pair(v1', v2)))
                                            | NONE => NONE
                                           )
    | step(D.First(t1)) = (case step(t1)
                      of SOME t1' => SOME(D.First(t1'))
                      | NONE => NONE
                    )
    | step(D.Second(t1)) = (case step(t1)
                      of SOME t1' => SOME(D.Second(t1'))
                      | NONE => NONE
                    )
				    
  fun eval t =
    let
      fun lp t =
	(case step t
	   of SOME t' => t :: lp t'
	    | NONE => [t])
    in
      lp t
    end		    

  fun result t1 = (case (List.last(eval t1))
                    of lastTerm => (case (isV lastTerm)
                                of true => Value(lastTerm)
                                | false => Stuck(lastTerm)
                              )
                    
                  )

end

(* final version *)
