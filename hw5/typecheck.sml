structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool
  val subtyHelper : ((string * Type.typ) list) * ((string * Type.typ) list) -> bool

  val listTypes : (string * Type.typ) list -> Type.typ list

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ

  val typeofHelper : TypeEnv.env * L23RR.term -> Type.typ


							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv

  fun mem("", []) = false 
    | mem(label, []) = false
    | mem(label, (frst :: rest)) = if (label = frst)
                                   then true 
                                   else mem(label, rest)

  fun listStrs([]) = []
    | listStrs((label, typ) :: rest) = (label :: listStrs(rest))
  
  fun listTypes([]) = []
    | listTypes((label, typ) :: rest) = (typ :: listTypes(rest))


  fun subtyHelper(typ1, []) = true
    | subtyHelper(typ1, typ2) = (case typ2
                                  of (str2, tm2) :: rest => (case (List.find((fn(label, tm) => (str2 = label))) typ1)
                                                                of SOME(label11, tm11) => if (subty(tm11, tm2))
                                                                                            then subty(T.Record(typ1), T.Record(rest))
                                                                                            else false
                                                                                          
                                                              | NONE => false
                                  )
                                  )

                                

		  
  and subty(T.Int, T.Int) = true
    | subty(T.Bool, T.Bool) = true
    | subty(T.Unit, T.Unit) = true
    | subty(T.Function(typ1, typ2), T.Function(typ3, typ4)) = if (subty(typ3, typ1))
                                                              then (if (subty(typ4, typ2))
                                                                    then true
                                                                    else false
                                                                   )
                                                              else false 
    | subty(T.Record(lst1), T.Record(lst2)) = if subtyHelper(lst1, lst2)
                                              then true
                                              else false
    | subty(t1, t2) = false

  fun commonSupertype(typ1, typ2) = if subty(typ1, typ2)
                                    then SOME(typ2)
                                    else (if subty(typ2, typ1)
                                            then SOME(typ1)
                                            else NONE
                                         )


  fun typeofHelper(gamma, L.Int(n)) = T.Int
    | typeofHelper(gamma, L.True) = T.Bool
    | typeofHelper(gamma, L.False) = T.Bool
    | typeofHelper(gamma, L.Unit) = T.Unit
    | typeofHelper(gamma, L.Var(v)) = (case (TypeEnv.lookup(gamma, v))
                                        of SOME(typ) => typ
                                        | _ => raise Fail "error, should have a type") 
    | typeofHelper(gamma, L.Lam(var, tau1, tm1)) = (case (TypeEnv.extend(gamma, var, tau1))
                                                          of gamma' => (case (typeofHelper(gamma', tm1))
                                                                          of tau2 => T.Function(tau1, tau2)
                                                                       )
                                                       )
    | typeofHelper(gamma, L.App(tm1, tm2)) = (case (typeofHelper(gamma, tm1))
                                                of T.Function(tau1, tau2) => (case (typeofHelper(gamma, tm2))
                                                                                of tau3 => if (subty(tau3, tau1))
                                                                                           then tau2
                                                                                           else raise Fail "error, tau3 not subty of tau1"
                                                                             )
                                                                             
                                             )                                                 
    | typeofHelper(gamma, L.Fix(tm)) = (case (typeofHelper(gamma, tm))
                                          of T.Function(t1, t2) => if (t1 = t2)
                                                                  then t1
                                                                  else raise Fail "types are not same"
                                       ) 
    | typeofHelper(gamma, L.Let(var, t1, t2)) = (case (typeofHelper(gamma, t1))
                                            of typ1 => (case (TypeEnv.extend(gamma, var, typ1))
                                                          of gamma' => (case (typeofHelper(gamma', t2))
                                                                          of typ2 => typ2
                                                                       )
                                                       )
                                          )                                                 
    | typeofHelper(gamma, L.Cond(t1, t2, t3)) = (case (typeofHelper (gamma, t1))
                                                  of T.Bool => (case (typeofHelper (gamma, t2))
                                                            of tau2 => (case (typeofHelper(gamma, t3))
                                                                          of tau3 => (case (commonSupertype(tau2,tau3))
                                                                                        of SOME(tau4) => tau4
                                                                                        | _ => raise Fail "no cmmon supertype"
                                                                                     )
                                                                        )
                                                         )
                                                  | _ => raise (Fail "Cond: first term isnt a boolean" )
                                                )
    
    | typeofHelper(gamma, L.Add(t1, t2)) = (case (typeofHelper (gamma, t1))
                                              of T.Int => (case (typeofHelper(gamma, t2))
                                                            of T.Int => T.Int
                                                            | _ => raise Fail "add: term2 not an int"
                                                          )
                                              | _ => raise Fail "add: term1 not an int"
                                           )
    | typeofHelper(gamma, L.Sub(t1, t2)) = (case (typeofHelper (gamma, t1))
                                              of T.Int => (case (typeofHelper(gamma, t2))
                                                            of T.Int => T.Int
                                                            | _ => raise Fail "sub: term2 not an int"
                                                          )
                                              | _ => raise Fail "sub: term1 not an int"
                                           )
    | typeofHelper(gamma, L.Mul(t1, t2)) = (case (typeofHelper (gamma, t1))
                                              of T.Int => (case (typeofHelper(gamma, t2))
                                                            of T.Int => T.Int
                                                            | _ => raise Fail "mul: term2 not an int"
                                                          )
                                              | _ => raise Fail "mul: term1 not an int"
                                           )
    
    | typeofHelper(gamma, L.Eq(t1, t2)) = (case (typeofHelper (gamma, t1))
                                              of T.Int => (case (typeofHelper(gamma, t2))
                                                            of T.Int => T.Bool
                                                            | _ => raise Fail "mul: term2 not an int"
                                                          )
                                              | _ => raise Fail "mul: term1 not an int"
                                           )
    | typeofHelper(gamma, L.LessThan(t1, t2)) = (case (typeofHelper (gamma, t1))
                                              of T.Int => (case (typeofHelper(gamma, t2))
                                                            of T.Int => T.Bool
                                                            | _ => raise Fail "mul: term2 not an int"
                                                          )
                                              | _ => raise Fail "mul: term1 not an int"
                                           )
    | typeofHelper(gamma, L.Not(tm)) = (case (typeofHelper(gamma, tm))
                                          of T.Bool => T.Bool
                                          | _ => raise Fail "not: term is not an int"
                                       )
                               
    | typeofHelper(gamma, L.Record(lst)) = T.Record(List.map((fn(label, tm) => (label, typeofHelper(gamma, tm)))) lst)

    | typeofHelper(gamma, L.Select(string, recrd)) = (case (typeofHelper(gamma, recrd))
                                                        of T.Record(typRecrd) => (case (List.find((fn(label, tm) => (string = label))) typRecrd)
                                                                          of SOME(newVal, newTyp) => newTyp
                                                                          | _ => raise Fail "nothing was found to select"
                                                                        )
                                                     )


                 

  fun typeof(tm) = typeofHelper(E.empty, tm)
	    
end
