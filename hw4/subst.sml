structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct

  structure U = ULC
  structure S = VarSet
		  
  fun fv(U.Var(varName)) = VarSet.ins(varName, VarSet.empty)
    | fv(U.App(t1, t2)) = VarSet.union(fv(t1), fv(t2))
    | fv(U.Lam(x, t1)) = VarSet.rem(x, fv(t1))
		  
  fun subst(var1, t1, U.Var(var2)) = if (var1 = var2)
                                     then t1
                                     else U.Var(var2)
    | subst(var1, t2, U.App(tApp1, tApp2)) = U.App(subst(var1, t2, tApp1), subst(var1, t2, tApp2))
    | subst(var1, t2, U.Lam(lamVar1, tLam2)) = if (var1 = lamVar1)
                                               then U.Lam(var1, tLam2)
                                               else (if (VarSet.mem(lamVar1, fv(t2)))
                                                      then 
                                                        let 
                                                          val y' = Fresh.var()
                                                        in   
                                                          subst(var1, t2, U.Lam(y', (subst(lamVar1, U.Var(y'), tLam2))))
                                                        end
                                                      else U.Lam(lamVar1, subst(var1, t2, tLam2))
                                                    )


end
