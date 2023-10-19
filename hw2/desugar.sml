structure Desugar : sig

  val desugar : Sugary.term -> Desugared.term

end = struct

  structure S = Sugary
  structure D = Desugared

  fun desugar S.True = D.Succ(D.Zero)
            | desugar(S.False) = D.Zero
            | desugar(S.Unit) = D.Zero
            | desugar(S.Nat(num)) = (case num
                                      of 0 => D.Zero
                                       | _ => D.Succ(desugar(S.Nat(num - 1)))
                                    )
            | desugar(S.Not(t)) = D.Cond(desugar(t), desugar(S.False), desugar(S.True))

            | desugar(S.And(t1, t2)) = D.Cond(desugar(t1), desugar(t2), desugar(S.False))

            | desugar(S.Or(t1, t2)) = D.Cond(desugar(t1), desugar(S.True), desugar(t2))

            | desugar(S.Xor(t1, t2)) = D.Cond(desugar(t1), desugar(S.Not(t2)), desugar(t2))

            | desugar(S.Less(t1, t2)) = D.Less(desugar(t1), desugar(t2))

            | desugar(S.Greater(t1, t2)) = D.Less(desugar(t2), desugar(t1))

            | desugar(S.LessEq(t1, t2)) = desugar(S.Or(S.Less(t1, t2), S.Eq(t1, t2)))

            | desugar(S.GreaterEq(t1, t2)) = desugar(S.Or(S.Greater(t1, t2), S.Eq(t1, t2)))

            | desugar(S.Add(t1, t2)) = D.Add(desugar(t1), desugar(t2))

            | desugar(S.Subtract(t1, t2)) = D.Subtract(desugar(t1), desugar(t2))

            | desugar(S.Cond(t1, t2, t3)) = D.Cond(desugar(t1), desugar(t2), desugar(t3))

            | desugar(S.Eq(t1, t2)) = D.Eq(desugar(t1), desugar(t2))

            | desugar(S.Pair(t1, t2)) = D.Pair(desugar(t1), desugar(t2))

            | desugar(S.First(t)) = D.First(desugar(t))

            | desugar(S.Second(t)) = D.Second(desugar(t))
      
end
