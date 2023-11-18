structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  (*fun isV(L.True) = true
    | isV(L.False) = true
    | isV(L.Unit) = true
    | isV(L.Int(n)) = true
    | isV(L.Lam (strng, typ, tm)) = true
    | isV(L.Record(lst)) = raise Fail "todo"
    | isV(_) = false *)

  fun eval(L.Int(n)) = L.Int(n)
    | eval(L.False) = L.False
    | eval(L.True) = L.True
    | eval(L.Unit) = L.Unit
    | eval (L.Var(x)) = raise Fail "should not have a variable"
    | eval(L.Lam (strng, typ, tm)) = L.Lam (strng, typ, tm)
    | eval(L.App(tm1, tm2)) = (case (eval(tm1))
                                  of L.Lam(x, tau1, tm11) => (case (eval(tm2))
                                                                of v2 => (case eval(Subst.subst(x, v2, tm11))
                                                                            of v3 => v3
                                                                         )
                                                             )
                                  | _ => raise Fail "term in application is not a lambda"
                              )
    | eval(L.Fix(tm)) = (case (eval(tm))
                                  of L.Lam(f, tau1, tm11) => (case eval(Subst.subst(f, L.Fix(L.Lam(f, tau1, tm11)), tm11))
                                                                of v1 => v1
                                                             )
                                  | _ => raise Fail "term in fix not a lambda"
                        )
    | eval(L.Let(var, tm1, tm2)) = (case (eval(tm1))
                                      of v1 => (case (eval(Subst.subst(var, v1, tm2)))
                                                  of v2 => v2
                                               )
                                   )
    | eval(L.Cond(t1, t2, t3)) = (case eval(t1)
                                    of L.True => (case (eval(t2))
                                                    of t2' => t2'
                                    )
                                    | L.False => (case (eval(t3))
                                                    of t3' => t3'
                                    )
                                    | _ => raise Fail "error 14 ??? first term not a boolean"
                                 )
    | eval(L.Add(t1, t2)) = (case eval(t1)
                                of L.Int(n1) => (case eval(t2)
                                                    of L.Int(n2) => L.Int(n1 + n2)
                                                    | _ => raise Fail "error4 ??? second term not a nat"
                                                )
                                | _ => raise Fail "error3 ??? first term not a nat"

                            )
    | eval(L.Sub(t1, t2)) = (case eval(t1)
                                of L.Int(n1) => (case eval(t2)
                                                    of L.Int(n2) => L.Int(n1 - n2)
                                                    | _ => raise Fail "error6 ??? second term not a nat"
                                                )
                                | _ => raise Fail "error5 ??? first term not a nat"

                            ) 
    | eval(L.Mul(t1, t2)) = (case eval(t1)
                                of L.Int(n1) => (case eval(t2)
                                                    of L.Int(n2) => L.Int(n1 * n2)
                                                    | _ => raise Fail "error8 ??? second term not a nat"
                                                )
                                | _ => raise Fail "error7 ??? first term not a nat"

                            )   
    | eval(L.Eq(t1, t2)) = (case eval(t1)
                                of L.Int(n1) => (case eval(t2)
                                                    of L.Int(n2) => if (n1=n2)
                                                                    then L.True
                                                                    else L.False
                                                    | _ => raise Fail "error10 ??? second term not a nat"
                                                )
                                | _ => raise Fail "error9 ??? first term not a nat"

                            ) 
    | eval(L.LessThan(t1, t2)) = (case eval(t1)
                                  of L.Int(n1) => (case eval(t2)
                                                    of L.Int(n2) => if (n1<n2)
                                                                    then L.True
                                                                    else L.False
                                                    | _ => raise Fail "error12 ??? second term not a nat"
                                                )
                                | _ => raise Fail "error11 ??? first term not a nat"

                            )  
    | eval(L.Not(tm)) = (case (eval(tm))
                            of L.True => L.False
                            | L.False => L.True
                            | _ => raise Fail "error 13 ??? term not a boolean"
                          )  
    | eval(L.Record(lst)) = L.Record(List.map((fn(label, tm) => (label, eval(tm)))) lst)

    | eval(L.Select(string, recrd)) = (case (eval(recrd))
                                          of L.Record(typRecrd) => (case (List.find((fn(label, tm) => (label = string))) typRecrd)
                                                                      of SOME(newL, newVal) => newVal
                                                                      | _ => raise Fail "nothing was found to select"
                                                                    )
                                                     )
      
                                       
		 
end
