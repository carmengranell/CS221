structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ

end = struct

  structure S = Sugary
  structure T = Type
  
  fun typeof (S.Nat(_)) = T.Nat
            | typeof(S.True) = T.Bool
            | typeof(S.False) = T.Bool
            | typeof(S.Unit) = T.Unit
            | typeof(S.Add(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Nat
                                                        | _ => raise (Fail "second term isnt a natural")
                                                      )
                                        | _ => raise (Fail "first term isnt a natural")
                                      )
            | typeof(S.Subtract(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Nat
                                                        | _ => raise (Fail "second term isnt a natural")
                                                      )
                                        | _ => raise (Fail "first term isnt a natural")
                                      )
            | typeof(S.Less(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      )
            | typeof(S.Greater(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      )
            | typeof(S.LessEq(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolena")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean?")
                                      )
            | typeof(S.GreaterEq(t1, t2)) = (case (typeof t1)
                                        of T.Nat => (case (typeof t2)
                                                        of T.Nat => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      )
            | typeof(S.Not(t1)) = (case (typeof t1)
                                    of T.Bool => T.Bool
                                    | _ => raise (Fail "term isn't a boolean, can't do ! operator")
                                  )
            | typeof(S.And(t1, t2)) = (case (typeof t1)
                                        of T.Bool => (case (typeof t2)
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      )
            | typeof(S.Or(t1, t2)) = (case (typeof t1)
                                        of T.Bool => (case (typeof t2)
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "second term isnt a boolean")
                                      )
            | typeof(S.Xor(t1, t2)) = (case (typeof t1)
                                        of T.Bool => (case (typeof t2)
                                                        of T.Bool => T.Bool
                                                        | _ => raise (Fail "second term isnt a boolean")
                                                      )
                                        | _ => raise (Fail "first term isnt a boolean")
                                      )
            | typeof(S.Cond(t1, t2, t3)) = (case (typeof t1)
                                            of T.Bool => (case (typeof t2)
                                                            of tau2 => if ((typeof t3) = tau2)
                                                                        then tau2
                                                                        else raise (Fail "t2 and t3 are not of the same type")
                                                         )
                                            | _ => raise (Fail "first term isnt a boolean" )
                                           )
            | typeof(S.Eq(t1, t2)) = (case (typeof t1)
                                        of tau1 => if ((typeof t2) = tau1)
                                                    then T.Bool
                                                    else raise (Fail "t1 and t2 are not of the same type")
                                     )
            | typeof(S.Pair(t1, t2)) = (case (typeof t1)
                                        of tau1 => (case (typeof t2)
                                                      of tau2 => T.Product(tau1, tau2)
                                                   )
                                     )
            | typeof(S.First(t)) = (case (typeof t)
                                      of T.Product(tau1, tau2) => tau1
                                      | _ => raise (Fail "term is not a pair")
                                   )
            | typeof(S.Second(t)) = (case (typeof t)
                                      of T.Product(tau1, tau2) => tau2
                                      | _ => raise (Fail "term is not a pair")
                                   )
            

end
