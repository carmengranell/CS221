structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof(gamma, S.Nat(_)) = T.Nat
    | typeof(gamma, S.True) = T.Bool  
    | typeof(gamma, S.False) = T.Bool
    | typeof(gamma, S.Unit) = T.Unit
    | typeof(gamma, S.Add(t1, t2)) = (case (typeof (gamma, t1))
                                      of T.Nat => (case (typeof (gamma, t2))
                                                    of T.Nat => T.Nat
                                                    | _ => raise Fail "second term isn't a natural"
                                                  )
                                      | _ => raise Fail "first term isn't a natural"
                                     )
    | typeof(gamma, S.Subtract(t1, t2)) = (case (typeof (gamma, t1))
                                            of T.Nat => (case (typeof (gamma, t2))
                                                          of T.Nat => T.Nat
                                                          | _ => raise Fail "second term isn't a natural"
                                                        )
                                            | _ => raise Fail "first term isn't a natural"
                                          ) 
    | typeof(gamma, S.Mul(t1, t2)) = (case (typeof (gamma, t1))
                                      of T.Nat => (case (typeof (gamma, t2))
                                                    of T.Nat => T.Nat
                                                    | _ => raise Fail "second term isn't a natural"
                                                  )
                                      | _ => raise Fail "first term isn't a natural"
                                     )
    | typeof(gamma, S.Pow(t1, t2)) = (case (typeof (gamma, t1))
                                      of T.Nat => (case (typeof (gamma, t2))
                                                    of T.Nat => T.Nat
                                                    | _ => raise Fail "second term isn't a natural"
                                                  )
                                      | _ => raise Fail "first term isn't a natural"
                                     )
    | typeof(gamma, S.Less(t1, t2)) = (case (typeof (gamma, t1))
                                      of T.Nat => (case (typeof (gamma, t2))
                                                    of T.Nat => T.Bool
                                                    | _ => raise Fail "second term isn't a bool"
                                                  )
                                      | _ => raise Fail "first term isn't a bool"
                                     )  
    | typeof(gamma, S.Greater(t1, t2)) = (case (typeof (gamma, t1))
                                            of T.Nat => (case (typeof (gamma, t2))
                                                          of T.Nat => T.Bool
                                                          | _ => raise Fail "second term isn't a bool"
                                                        )
                                            | _ => raise Fail "first term isn't a bool"
                                         )
    | typeof(gamma, S.LessEq(t1, t2)) = (case (typeof (gamma, t1))
                                          of T.Nat => (case (typeof (gamma, t2))
                                                        of T.Nat => T.Bool
                                                        | _ => raise Fail "second term isn't a bool"
                                                      )
                                          | _ => raise Fail "first term isn't a bool"
                                         )
    | typeof(gamma, S.GreaterEq(t1, t2)) = (case (typeof (gamma, t1))
                                              of T.Nat => (case (typeof (gamma, t2))
                                                            of T.Nat => T.Bool
                                                            | _ => raise Fail "second term isn't a bool"
                                                          )
                                              | _ => raise Fail "first term isn't a bool"
                                           )
    | typeof(gamma, S.Not(t1)) = (case (typeof (gamma, t1))
                                    of T.Bool => T.Bool
                                    | _ => raise (Fail "term isn't a boolean, can't do ! operator")
                                  )
    | typeof(gamma, S.And(t1, t2)) = (case (typeof (gamma, t1))
                                        of T.Bool => (case (typeof (gamma, t2))
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      ) 
    | typeof(gamma, S.Or(t1, t2)) = (case (typeof (gamma, t1))
                                        of T.Bool => (case (typeof (gamma, t2))
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      ) 
    | typeof(gamma, S.Xor(t1, t2)) = (case (typeof (gamma, t1))
                                        of T.Bool => (case (typeof (gamma, t2))
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      ) 
    | typeof(gamma, S.Cond(t1, t2, t3)) = (case (typeof (gamma, t1))
                                            of T.Bool => (case (typeof (gamma, t2))
                                                            of tau2 => if ((typeof (gamma, t3)) = tau2)
                                                                        then tau2
                                                                        else raise (Fail "t2 and t3 are not of the same type")
                                                         )
                                            | _ => raise (Fail "first term isnt a boolean" )
                                           )
    | typeof(gamma, S.Eq(t1, t2)) = (case (typeof (gamma, t1))
                                        of T.Nat => if ((typeof (gamma, t2)) = T.Nat)
                                                    then T.Bool
                                                    else raise (Fail "t2 is not a nat")
                                        | _ => raise Fail "t1 is not a nat"
                                     )
    | typeof(gamma, S.Pair(t1, t2)) = (case (typeof (gamma, t1))
                                        of tau1 => (case (typeof (gamma, t2))
                                                      of tau2 => T.Product(tau1, tau2)
                                                   )
                                     )
    | typeof(gamma, S.First(t)) = (case (typeof (gamma, t))
                                      of T.Product(tau1, tau2) => tau1
                                      | _ => raise (Fail "term is not a pair")
                                   )
    | typeof(gamma, S.Second(t)) = (case (typeof (gamma, t))
                              of T.Product(tau1, tau2) => tau2
                              | _ => raise (Fail "term is not a pair")
                            )
    | typeof(gamma, S.Var(v)) = (case (TypeEnv.lookup(gamma, v))
                            of SOME(typ) => typ
                            | _ => raise Fail "error, should have a type")

    | typeof(gamma, S.Let(var, t1, t2)) = (case (typeof(gamma, t1))
                                            of typ1 => (case (TypeEnv.extend(gamma, var, typ1))
                                                          of gamma' => (case (typeof(gamma', t2))
                                                                          of typ2 => typ2
                                                                       )
                                                       )
                                          )                                

end
