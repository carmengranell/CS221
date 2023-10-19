structure Compile : sig

  val code : string -> Eval.norm * Type.typ
  val file : string -> Eval.norm * Type.typ
			 
end = struct
 
  fun code program =
    let
      val tokens  = Scan.scan program
      val _ = print "scanned\n"
      val sweet   = Parse.parse tokens
      val _ = print "parsed\n"
      val tau     = TypeCheck.typeof sweet
      val _ = print "typeChecked\n"
      val unsweet = Desugar.desugar sweet
      val _ = print "desugared\n"
      val result  = Eval.result unsweet
      val _ = print "eval result\n"
    in
      (result, tau)
    end

  fun file filename =
    let
      val program = ReadFile.toString filename
    in
      code program
    end

end
