structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.expect(Scan.scan "ZS", [T.Z, T.S], "scan00")
      val _ = Check.expect(Scan.scan "PSZ+Z", [T.P, T.S, T.Z, T.Plus, T.Z], "scan000")
      val _ = Check.expect(Scan.scan " Z +      + Z", [T.Z, T.Plus, T.Plus, T.Z], "scan0000")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan000")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.expect (Parse.parse [T.T], A.True, "parse000")
      val _ = Check.exn (fn () => Parse.parse [T.T, T.Plus], "badParse00")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      val _ = Check.expect (Eval.eval (A.Succ(A.Pred(A.Zero))), [A.Succ(A.Pred(A.Zero)), A.Succ(A.Zero)], "eval00")
      (* write more eval tests here *)
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
      (* write more eval tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
