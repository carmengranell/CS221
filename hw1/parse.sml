structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next (T.Z :: toks) = SOME(A.Zero, toks)
      | next(T.T :: toks) = SOME(A.True, toks)
      | next(T.F :: toks) = SOME(A.False, toks)
      | next(T.S :: toks) = (case (next toks)
                              of SOME (t1, toks') => SOME(A.Succ t1, toks')
                              | NONE => raise (Fail "ended too soon"))
      | next(T.P :: toks) = (case (next toks)
                              of SOME (t1, toks') => SOME(A.Pred t1, toks')
                              | NONE => raise (Fail "ended too soon"))
      | next(T.LBrack :: toks) = (case (next toks)
                                    of SOME(t1, T.Plus :: toks') => (case (next toks')
                                                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Add(t1, t2), toks'')
                                                                        | SOME(t2, toks'') => raise (Fail "no right bracket")
                                                                        | _ => raise (Fail "not valid"))
                                    | SOME(t1, T.Minus :: toks') => (case (next toks')
                                                                            of SOME(t2, T.RBrack :: toks'') => SOME(A.Subtract(t1, t2), toks'')
                                                                            | SOME(t2, toks'') => raise(Fail "no right bracket")
                                                                            | _ => raise (Fail "not valid"))
                                    | SOME(t1, T.GreaterThan :: toks') => (case (next toks')
                                                                            of SOME(t2, T.RBrack :: toks'') => SOME(A.Greater(t1, t2), toks'')
                                                                            | SOME(t2, toks'') => raise(Fail "no right bracket")
                                                                            | _ => raise (Fail "not valid"))
                                    | SOME(t1, T.LessThan :: toks') => (case (next toks')
                                                                            of SOME(t2, T.RBrack :: toks'') => SOME(A.Less(t1, t2), toks'')
                                                                            | SOME(t2, toks'') => raise(Fail "no right bracket")
                                                                            | _ => raise (Fail "not valid"))                                       
                                    | SOME(t1, T.DoubleAmpersand :: toks') => (case (next toks')
                                                                            of SOME(t2, T.RBrack :: toks'') => SOME(A.And(t1, t2), toks'')
                                                                            | SOME(t2, toks'') => raise(Fail "no right bracket")
                                                                            | _ => raise (Fail "not valid")) 
                                    | SOME(t1, T.DoublePipe :: toks') => (case (next toks')
                                                                            of SOME(t2, T.RBrack :: toks'') => SOME(A.Or(t1, t2), toks'')
                                                                            | SOME(t2, toks'') => raise(Fail "no right bracket")
                                                                            | _ => raise (Fail "not valid"))
                                    | SOME(t1, T.QuestionMark :: toks') => (case (next toks')
                                                                              of SOME(t2, T.Colon :: toks'') => (case (next toks'')
                                                                                                                  of SOME(t3, T.RBrack :: toks''') => SOME(A.Cond(t1, t2, t3), toks''')
                                                                                                                  | SOME(t3, toks''') => raise(Fail "no right bracket")
                                                                                                                  | _ => raise (Fail "not valid"))
                                                                              | SOME(t2, toks'') => raise (Fail "missing colon")
                                                                              | _ => raise (Fail "not valid"))
                                    | SOME(_, toks') => raise (Fail("missing operator"))
                                    | NONE => raise (Fail "ended too soon")) 
      | next(_) = raise (Fail "not valid, missing a term")                             


  fun parse [] = raise Fail("no valid AST term")
    | parse toks' = 
        (case (next toks')
            of SOME(tm, []) => tm
            | SOME(tm,toks') => raise (Fail "too many tokens")
            | NONE => raise (Fail "no valid AST term")
        )
		     
end
