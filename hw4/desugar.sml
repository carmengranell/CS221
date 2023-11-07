structure Desugar : sig

  val desugar : Sugary.term -> ULC.term
  val succs : int -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  fun succs(0) = U.App(U.Var("s"), U.Var("z"))
            | succs(n) = U.App(U.Var("s"), succs(n -1))

  val tru = ULC.Lam("t", ULC.Lam("f", ULC.Var("t")))
  val fls = U.Lam("t", U.Lam("f", U.Var("f")))
  val plus = U.Lam("m", U.Lam("n", U.Lam("s", U.Lam("z", U.App(U.App(U.Var("m"), U.Var("s")), U.App(U.App(U.Var("n"), U.Var("s")), U.Var("z")))))))
  val church0 = U.Lam("s", U.Lam("z", U.Var("z")))
  val church1 = U.Lam("s", U.Lam("z", U.App(U.Var("s"), U.Var("z"))))
  val pair = U.Lam("f", U.Lam("s", U.Lam("b", U.App(U.App(U.Var("b"), U.Var("f")), U.Var("s")))))
  val fst = U.Lam("p", U.App(U.Var("p"), tru))
  val snd = U.Lam("p", U.App(U.Var("p"), fls))
  val zz = U.App(U.App(pair, church0), church0)
  val ss = U.Lam("p", U.App(U.App(pair, U.App(snd, U.Var("p"))), U.App(U.App(plus, church1), U.App(snd, U.Var("p")))))
  val prd = U.Lam("m", U.App(fst, U.App(  U.App(U.Var("m"), ss), zz)))
  val subtract = U.Lam("m", U.Lam("n", U.App(U.App(U.Var("n"), prd), U.Var("m"))))
  val times = U.Lam("m", U.Lam("n", U.Lam("s",  U.App(U.Var("m"), U.App(U.Var("n"), U.Var("s"))))))
  val power = U.Lam("m", U.Lam("n",  U.App(  U.App(U.Var("n"),  U.App(times, U.Var("m"))  )   , church1))) 
  val not = U.Lam("b", U.App(U.App(U.Var("b"), fls), tru))
  val nd = U.Lam("b",  U.Lam("c", U.App(U.App(U.Var("b"), U.Var("c")), fls)))
  val or = U.Lam("b",  U.Lam("c", U.App(U.App(U.Var("b"), tru), U.Var("c"))))
  val cond = U.Lam("l", U.Lam("m", U.Lam("n", U.App(U.App(U.Var("l"), U.Var("m")), U.Var("n")))))
  val iszro = U.Lam("m", U.App(U.App(U.Var("m"), U.Lam("x", fls)), tru))
  val equal = U.Lam("m", U.Lam("n", U.App( U.App(nd, U.App(iszro, U.App(U.App(U.Var("m"), prd), U.Var("n")))), U.App(iszro, U.App(U.App(U.Var("n"), prd), U.Var("m"))))))


  fun succs(0) = U.App(U.Var("s"), U.Var("z"))
            | succs(n) = U.App(U.Var("s"), succs(n -1))

  fun desugar (S.Nat(num)) = (case (num)
                                of 0 => U.Lam("s", U.Lam("z", U.Var("z")))
                                | n => U.Lam("s", U.Lam("z", succs(n - 1)))
                             )
    | desugar(S.True) = tru

    | desugar(S.False) = fls

    | desugar(S.Unit) = church0

    | desugar(S.Add(t1, t2)) = U.App(U.App(plus, desugar(t1)), desugar(t2))

    | desugar(S.Subtract(t1, t2)) = U.App(U.App(subtract, desugar(t1)), desugar(t2))

    | desugar(S.Mul(t1, t2)) = U.App(U.App(times, desugar(t1)), desugar(t2))

    | desugar(S.Pow(t1, t2)) = U.App(U.App (power, desugar(t1)), desugar(t2)) (* fix *)

    | desugar(S.Less(t1, t2)) = desugar(S.And(S.Eq(S.Subtract(t1, t2), S.Nat(0)), S.Not(S.Eq(t1, t2))))

    | desugar(S.Greater(t1, t2)) = desugar(S.And(S.Eq(S.Subtract(t2, t1), S.Nat(0)), S.Not(S.Eq(t1, t2))))

    | desugar(S.LessEq(t1, t2)) = desugar(S.Or(S.Less(t1, t2), S.Eq(t1, t2)))

    | desugar(S.GreaterEq(t1, t2)) = desugar(S.Or(S.Greater(t1, t2), S.Eq(t1, t2)))
    
    | desugar(S.Not(t)) = U.App(not, desugar(t)) 

    | desugar(S.And(t1, t2)) = U.App(U.App(nd, desugar(t1)), desugar(t2))

    | desugar(S.Or(t1, t2)) = U.App(U.App(or, desugar(t1)), desugar(t2))

    | desugar(S.Xor(t1, t2)) = desugar(S.Cond(t1, S.Not(t2), t2))

    | desugar(S.Cond(t1, t2, t3)) =  U.App(U.App(U.App(cond, desugar(t1)), desugar(t2)), desugar(t3))

    | desugar(S.Eq(t1, t2)) = U.App(U.App(equal, desugar(t1)), desugar(t2))

    | desugar(S.Pair(t1, t2)) = U.App(U.App(pair, desugar(t1)), desugar(t2))

    | desugar(S.First(t)) = U.App(fst, desugar(t)) 

    | desugar(S.Second(t)) = U.App(snd, desugar(t)) 

    | desugar(S.Var(v)) = U.Var(v)

    | desugar(S.Let(var, t1, t2)) = U.App(U.Lam(var, desugar(t2)), desugar(t1)) 



end
