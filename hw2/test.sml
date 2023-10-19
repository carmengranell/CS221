structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
    let
      val _ = Check.expect (Scan.scan "2", [T.Nat 2], "scan2")
      val _ = Check.expect (Scan.scan "00002", [T.Nat 2], "scan2")
      val _ = Check.expect (Scan.scan "12", [T.Nat 12], "scan12")
      val _ = Check.expect (Scan.scan "  45", [T.Nat 45], "scan45")
      val _ = Check.expect (Scan.scan "TF 45 [4<3]", [T.T, T.F, T.Nat 45, T.LBrack, T.Nat 4, T.LessThan, T.Nat 3, T.RBrack], "scan00")
      (* check 1 (works), 123(works), 1TF23(works), TF1TF23TF345, 1234TFF , 1234TFF*)
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun typ () =
    let
      val _ = Check.expect (TypeCheck.typeof (S.Nat 12), Type.Nat, "type12") 
    in
      TextIO.print "type tests done\n"
    end

  fun desugar () =
    let
      val desugar = Desugar.desugar
      val _ = Check.expect (desugar (S.Nat 0), D.Zero, "desugar0")
    in
      TextIO.print "desugar tests done\n"
    end
			            
  fun eval () =
    let
      val _ = Check.expect (Eval.result D.Zero, Eval.Value D.Zero, "eval0")
      (* write more eval tests here *)
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      fun value typ program result =
	Check.expect (Compile.code program, (E.Value result, typ), "compile"^program)
      val natval = value Type.Nat
      val boolval = value Type.Bool
      val _ = natval "0" D.Zero 
      val _ = Check.expect (Compile.code "0",
                            (E.Value D.Zero, Type.Nat),
                            "compile0")
      val _ = Check.expect (Compile.code "1",
                            (E.Value (D.Succ D.Zero), Type.Nat),
                            "compile1")
      val _ = Check.expect (Compile.code "F",
                            (E.Value D.Zero, Type.Bool),
                            "compileF")
      (* write more compile tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = typ ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
