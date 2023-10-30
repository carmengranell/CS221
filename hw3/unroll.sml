structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term
  val replace : string * Sweetl.term * Sweetl.term -> Sweetl.term 

end = struct

  structure S = Sweetl

  fun replace (abbr, abbr_tm, tm) = (case (tm)
                              of S.Abbr(x) => if (abbr = x)
                                              then abbr_tm
                                              else tm
                              | S.App(x, y) => S.App(replace(abbr, abbr_tm, x), replace(abbr, abbr_tm, y))
                              | _ => tm
                           )

  fun unroll (S.Prog ([], tm)) = tm
      | unroll (S.Prog(abbrevs, tm)) = (case (abbrevs)
                                          of ((abbr_str, abbr_tm) :: rest) => unroll(S.Prog(rest, replace(abbr_str, abbr_tm, tm)))
                                       )
				 
end
