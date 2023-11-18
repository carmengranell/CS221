structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)
  val check : L23RR.term -> L23RR.term

  val listRecs : (string * L23RR.term) list -> string list 
  
end = struct

  structure L = L23RR

  fun mem(label, []) = false
    | mem(label, (frst :: rest)) = if (label = frst)
                                   then true 
                                   else mem(label, rest)

  fun listRecs([]) = []
    | listRecs((label, typ) :: rest) = (label :: listRecs(rest))

  fun check(L.Record([])) = raise Fail "empty record"
    | check(L.Record(lst)) = (case (lst)
                                of (label, tm) :: [] => (case (lst)
                                                          of (label, tm) :: [] => (case (check(tm))
                                                                                    of tm => L.Record(lst)
                                                                                  )
                                                          | _ => raise Fail "not correct"
                                                        )
                                | (label, tm) :: (label2, tm2) :: rest => if mem(label, listRecs((label2, tm2) :: rest))                         
                                                                          then raise Fail "error: same label used more than once"
                                                                          else (case (check(tm))
                                                                                  of tm => (case (check(tm2))
                                                                                               of tm2 => (case (check(L.Record ((label2, tm2) :: rest)))
                                                                                                            of (L.Record ((label2, tm2) :: rest)) => (L.Record (lst)) 
                                                                                                            
                                                                                                        ) 
                                                                                            )

                                                                                  )
                                | _ => raise Fail "fail"            
                             )
                             
    | check(L.Int(n)) = L.Int(n)
    | check(L.True) = L.True
    | check(L.False) = L.False
    | check(L.Unit) = L.Unit
    | check(L.Var(v)) = L.Var(v)
    | check(L.Lam(string, typ, tm)) = L.Lam(string, typ, check(tm))
    | check(L.App(tm1, tm2)) = L.App(check(tm1), check(tm2))
    | check(L.Fix(tm)) = L.Fix(check(tm))
    | check(L.Let(string, tm1, tm2)) = L.Let(string, check(tm1), check(tm2))
    | check(L.Cond(tm1, tm2, tm3)) = L.Cond(check(tm1), check(tm2), check(tm3))
    | check(L.Add(tm1, tm2)) = L.Add(check(tm1), check(tm2))
    | check(L.Sub(tm1, tm2)) = L.Sub(check(tm1), check(tm2))
    | check(L.Mul(tm1, tm2)) = L.Mul(check(tm1), check(tm2))
    | check(L.Eq(tm1, tm2)) = L.Eq(check(tm1), check(tm2))
    | check(L.LessThan(tm1, tm2)) = L.LessThan(check(tm1), check(tm2))
    | check(L.Not(tm)) = L.Not(check(tm))
    | check(L.Select(string, tm)) = L.Select(string, check(tm))
               
                                                  
end
