structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
  
end = struct

  type env = (string * Type.typ) list 
	       
  val empty = [] 

  fun lookup([], var:string) = NONE
    | lookup(((firstVar, firstType) :: rest), var) = if (var = firstVar)
                                                   then SOME(firstType)
                                                   else (lookup(rest, var))

  fun extend(gamma, variable, typ1) = ((variable, typ1) :: gamma)
					  
end
