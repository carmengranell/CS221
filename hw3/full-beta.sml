structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC

  fun step(U.App(U.Lam(var1, t1), t2)) =  SOME(Subst.subst(var1, t2, t1))
    | step(U.App(t1, t2)) = (case (step t1)
                              of SOME t1' => SOME(U.App(t1', t2))
                              | NONE => (case (step t2)
                                          of SOME t2' => SOME(U.App(t1, t2'))
                                          | NONE => NONE
                                        ))
    | step(U.Lam(strr, t1)) = (case (step t1)
                                of SOME t1' => SOME(U.Lam(strr, t1'))
                                | NONE => NONE
                              )
    | step(_) = NONE

end
