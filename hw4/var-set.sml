structure VarSet :> sig

  type set

  val empty  : set
  val mem    : string * set -> bool
  val ins    : string * set -> set
  val rem    : string * set -> set
  val union  : set * set -> set

(* the following operations aren't necessary for substitution *)
(* it just seemed sad not to have them for sets *)
(* they also make testing much easier *)

  val sing   : string -> set (* singleton set *)
  val size   : set -> int
  val subset : set * set -> bool
  val equals : set * set -> bool
			      
  val toList : set -> string list

end = struct

  type set = string list 
  val empty = []  

  fun mem ("", _) = false
    | mem (item, []) = false
    | mem (item, (frst :: rest)) = if (item = frst)
                                   then true
                                   else mem(item, rest)

  fun ins("", all) = all
    | ins(item, []) = [item]
    | ins(item, all) = if mem(item, all)
                       then all
                       else (item :: all)

  fun rem("", all) = all
    | rem(item, []) = []
    | rem(item, (frst :: rest)) = if (item = frst)
                                  then rest
                                  else (frst :: (rem(item, rest)))


  fun union([], set2) = set2
      | union(set1, []) = set1
      | union(set1, set2) = (case (set1)
                              of (frst :: rest) => if (mem(frst, set2))
                                                   then union(rest, set2)
                                                   else union(rest, frst :: set2)
                              | _ => raise Fail "nothing should happen"
                            ) 
  
  
  fun sing _   = raise Fail "todo"
  fun size _   = raise Fail "todo"
  fun subset _ = raise Fail "todo"
  fun equals _ = raise Fail "todo"
  fun toList _ = raise Fail "todo"
      				  
end
