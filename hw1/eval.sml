structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV A.Zero = true
            | isNV(A.Succ t1) = (case isNV(t1)
                              of true => true
                              | false => false)                      
            | isNV(_) = false

  fun isV A.True = true
        | isV(A.False) = true
        | isV(t1) = (case (isNV t1)
                  of true => true
                  | false => false)

		 
  fun step A.True = NONE
      | step A.False = NONE
      | step A.Zero = NONE
      | step (A.Cond(A.True, t2, _)) = SOME(t2)
      | step(A.Cond(A.False, _, t3)) = SOME(t3)
      | step(A.Cond(t1, t2, t3)) = (case (step t1)
                                      of SOME t1' => SOME(A.Cond(t1', t2, t3))
                                      | NONE => NONE)
      | step(A.Pred A.Zero) = SOME A.Zero
      | step(A.Pred t1) = if (isNV t1)
                          then (case t1
                                  of A.Succ nv1 => SOME nv1
                                  | A.Zero => raise Fail "will never reach (1)"
                                  | _ => raise Fail "will never reach (2)")
                          else (case (step t1)
                                  of SOME t1' => SOME(A.Pred t1')
                                  | NONE => NONE)
      | step(A.Succ t1) = (case (step t1)
                            of SOME t1' => SOME(A.Succ t1')
                            | NONE => NONE)
      | step(A.Add(A.Zero, t2)) = SOME t2
      | step(A.Add((A.Succ t1), t2)) = if (isNV t1)
                                        then SOME(A.Add(t1, A.Succ(t2)))
                                        else (case (step t1)
                                               of SOME t1' => SOME(A.Add(A.Succ(t1'), t2))
                                               | NONE => NONE)
      | step(A.Add(t1, t2)) = (case (step t1)
                                      of SOME t1' => SOME(A.Add(t1', t2))
                                      | NONE => SOME(A.Add(t1, t2)))
      | step(A.Subtract(A.Zero, nv2)) = if (isNV nv2)
                                       then SOME(A.Zero)
                                        else (case (step nv2)
                                              of SOME t2' => SOME(A.Subtract(A.Zero, t2'))
                                              | NONE => NONE)
      | step(A.Subtract(nv1, A.Zero)) = if (isNV nv1)
                                       then SOME(nv1)
                                        else (case (step nv1)
                                                of SOME t1' => SOME(A.Subtract(t1', A.Zero))
                                                | NONE => NONE) 
      | step(A.Subtract((A.Succ nv1), (A.Succ nv2))) =  (case (isNV nv1)
                                                    of true => (case (isNV nv2)
                                                          of true => SOME(A.Subtract(nv1, nv2))
                                                          | false => (case (step nv2)
                                                                    of SOME t2' => SOME(A.Subtract(A.Succ(nv1), A.Succ(t2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step nv1)
                                                           of SOME t1' => SOME(A.Subtract(A.Succ(t1'), A.Succ(nv2)))
                                                           | NONE => NONE))                                                 
      | step(A.Subtract(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(A.Subtract(t1, t2'))
                                      | NONE => SOME(A.Subtract(t1, t2)))
                              else (case (step t1)
                                      of SOME t1' => SOME(A.Subtract(t1', t2))
                                      | NONE => SOME(A.Subtract(t1, t2)))
      | step(A.Less(A.Zero, A.Zero)) = SOME(A.False)
      | step(A.Less(A.Zero, A.Succ(nv2))) = if (isNV nv2)
                                            then SOME(A.True)
                                            else NONE
      | step(A.Less(nv1, A.Zero)) = if (isNV nv1)
                                    then SOME(AST.False)
                                    else NONE
      | step(A.Less(A.Succ(nv1), A.Succ (nv2))) = (case (isNV nv1)
                                                    of true => (case (isNV nv2)
                                                          of true => SOME(A.Less(nv1, nv2))
                                                          | false => (case (step nv2)
                                                                    of SOME t2' => SOME(A.Less(A.Succ(nv1), A.Succ(t2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step nv1)
                                                           of SOME t1' => SOME(A.Less(A.Succ(t1'), A.Succ(nv2)))
                                                           | NONE => NONE))
      | step(A.Less(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(A.Less(t1, t2'))
                                      | NONE => SOME(A.Less(t1, t2)))
                              else (case (step t1)
                                      of SOME t1' => SOME(A.Less(t1', t2))
                                      | NONE => SOME(A.Less(t1, t2)))

      | step(A.Greater(A.Zero, nv2)) = if (isNV nv2)
                                            then SOME(A.False)
                                            else (case (step nv2)
                                                  of SOME t2' => SOME(A.Greater(A.Zero, t2'))
                                                  | NONE => NONE)
      | step(A.Greater(A.Succ(nv1), A.Zero)) = if (isNV nv1)
                                            then SOME(A.True)
                                            else (case (step nv1)
                                                  of SOME t1' => SOME(A.Greater(A.Succ(t1'), A.Zero))
                                                  | NONE => NONE)
      | step(A.Greater(A.Succ(nv1), A.Succ(nv2))) = (case (isNV nv1)
                                                    of true => (case (isNV nv2)
                                                          of true => SOME(A.Greater(nv1, nv2))
                                                          | false => (case (step nv2)
                                                                    of SOME t2' => SOME(A.Greater(A.Succ(nv1), A.Succ(t2')))
                                                                    | NONE => NONE))
                                                    | false => (case (step nv1)
                                                           of SOME t1' => SOME(A.Greater(A.Succ(t1'), A.Succ(nv2)))
                                                           | NONE => NONE))
      | step(A.Greater(t1, t2)) = if (isV t1)
                              then (case (step t2)
                                      of SOME t2' => SOME(A.Greater(t1, t2'))
                                      | NONE => NONE)
                              else (case (step t1)
                                      of SOME t1' => SOME(A.Greater(t1', t2))
                                      | NONE => NONE)
      | step(A.And(A.True, t2)) = SOME(t2)
      | step(A.And(A.False, t2)) = SOME(A.False)
      | step(A.And(t1, t2)) = (case (step t1)
                                of SOME t1' => SOME(A.And(t1', t2))
                                | NONE => NONE)
      | step(A.Or(A.True, t2)) = SOME(A.True)
      | step(A.Or(A.False, t2)) = SOME(t2)
      | step(A.Or(t1, t2)) = (case (step t1)
                                of SOME t1' => SOME(A.Or(t1', t2))
                                | NONE => NONE)
                      



  fun eval term = 
    (case (step term)
                  of SOME(term') => term :: eval(term')
                  | NONE => [term])
	 
end

(*last version*)