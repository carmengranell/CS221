structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term
  val succs : int -> ULC.term

end = struct

  structure S = Sweetl
  structure U = ULC

  fun succs(0) = U.App(U.Var("s"), U.Var("z"))
            | succs(n) = U.App(U.Var("s"), succs(n -1))

  fun desugar (S.Var(x)) = U.Var(x)

    | desugar (S.App(t1, t2)) = U.App(desugar(t1), desugar(t2))
    
    | desugar (S.Lam(str, t2)) = U.Lam(str, desugar(t2))


    | desugar (S.Nat(num)) = (case (num)
                                of 0 => U.Lam("s", U.Lam("z", U.Var("z")))
                                | n => U.Lam("s", U.Lam("z", succs(n - 1)))
                             )


    | desugar (S.Tru) = U.Lam("t", U.Lam("f", U.Var("t")))

    | desugar (S.Fls) = U.Lam("t", U.Lam("f", U.Var("f")))

    | desugar (S.ID(str)) = U.Lam(str, U.Var(str))

    | desugar (S.Abbr(str)) = raise (Fail "error: shouldn't have abbrev after unrolling")


end
