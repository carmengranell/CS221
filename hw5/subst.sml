structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst(var1, t2, L.Int(n)) = L.Int(n)
    | subst(var1, t2, L.True) = L.True
    | subst(var1, t2, L.False) = L.False
    | subst(var1, t2, L.Unit) = L.Unit
    | subst(var1, t2, L.Var(var2)) = if (var1 = var2)
                                      then t2
                                      else L.Var(var2)
    | subst(var1, t2, L.App(tApp1, tApp2)) = L.App(subst(var1, t2, tApp1), subst(var1, t2, tApp2))
    | subst(var1, t2, L.Lam(lamVar1, typ, tLam2)) = if (var1 = lamVar1)
                                               then L.Lam(var1, typ, tLam2)
                                               else L.Lam(lamVar1, typ, subst(var1, t2, tLam2))
    | subst(var1, t2, L.Fix(tm)) = L.Fix(subst(var1, t2, tm))
    | subst (var1, t2, L.Let (str, tm1, tm2)) = if (var1 = str) 
                                                then L.Let(var1, (subst (str, t2, tm1)), tm2) 
                                                else L.Let(str, (subst(var1, t2, tm1)), (subst(var1, t2, tm2))) 
    | subst(var1, t2, L.Cond(tm1, tm2, tm3)) = L.Cond(subst(var1, t2, tm1), subst(var1, t2, tm2), subst(var1, t2, tm3))
    | subst(var1, t2, L.Add(tm1, tm2)) = L.Add(subst(var1, t2, tm1), subst(var1, t2, tm2))
    | subst(var1, t2, L.Sub(tm1, tm2)) = L.Sub(subst(var1, t2, tm1), subst(var1, t2, tm2))
    | subst(var1, t2, L.Mul(tm1, tm2)) = L.Mul(subst(var1, t2, tm1), subst(var1, t2, tm2))
    | subst(var1, t2, L.Eq(tm1, tm2)) = L.Eq(subst(var1, t2, tm1), subst(var1, t2, tm2))
    | subst(var1, t2, L.LessThan(tm1, tm2)) = L.LessThan(subst(var1, t2, tm1), subst(var1, t2, tm2))
    | subst(var1, t2, L.Not(tm1)) = L.Not(subst(var1, t2, tm1))
    | subst(var1, t2, L.Record(lst)) = L.Record(List.map((fn(label, tm) => (label, subst(var1, t2, tm)))) lst)
    | subst(var1, t2, L.Select(string, tm)) = L.Select(string, subst(var1, t2, tm))

                                                  

end
