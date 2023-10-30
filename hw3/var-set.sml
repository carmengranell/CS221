structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = []  (* <== Change this to something consistent with the new set type. *)

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
				      
end
