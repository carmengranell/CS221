structure CBV : sig

  val step : ULC.term -> ULC.term option
  val isV : ULC.term -> bool

end = struct

  structure U = ULC

  fun isV(U.Lam(_, _)) = true
      | isV(_) = false

  fun step(U.App(U.Lam(var1, t1), v2)) = if (isV v2)
                                           then SOME(Subst.subst(var1, v2, t1))
                                           else NONE
    | step(U.App(t1, t2)) = if (isV t1)
                            then (case (step t2)
                                    of SOME t2' => SOME(U.App(t1, t2'))
                                    | NONE => NONE
                                 )
                            else (case (step t1)
                                    of SOME t1' => SOME(U.App(t1', t2))
                                    | NONE => NONE
                                 )
      | step(_) = NONE

end
