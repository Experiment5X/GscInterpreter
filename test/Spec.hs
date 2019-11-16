import Test.HUnit
import LanguageStructure
import Parser


parseStatementT :: String -> Stmt
parseStatementT s = case parseStatement s of
                      (Left _)  -> error ("parse error: " ++ s)
                      (Right e) -> e

parseStatementTests :: Test
parseStatementTests =
           TestList [ parseStatementT "a = b[1];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Var (LValue (Qualifier []) [LValueComp "b" [IntLit 1]]))
                    , parseStatementT "a = 0.5;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (FloatLit 0.5)
                    , parseStatementT "a = true;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (BoolLit True)
                    , parseStatementT "a = [];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [])
                    , parseStatementT "a = [1,2,3];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [IntLit 1,IntLit 2,IntLit 3])
                    , parseStatementT "a = [f(), 1, 0.3, \"adam\"];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "f" []])) [],IntLit 1,FloatLit 0.3,StringLit "adam"])
                    , parseStatementT "a = (1,2,3);" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Vec3Lit (IntLit 1) (IntLit 2) (IntLit 3))
                    , parseStatementT "a += f();" ~?= PlusEquals (LValue (Qualifier []) [LValueComp "a" []]) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "f" []])) [])
                    , parseStatementT "--index;" ~?= AssignExprStmt (PreDec (Var (LValue (Qualifier []) [LValueComp "index" []])))
                    , parseStatementT "yo = other\\module::var;" ~?= Assign (LValue (Qualifier []) [LValueComp "yo" []]) (Var (LValue (Qualifier ["other","module"]) [LValueComp "var" []]))
                    , parseStatementT "awardXp = self.pers[\"killstreaks\"][0].awardXp;" ~?= Assign (LValue (Qualifier []) [LValueComp "awardXp" []]) (Var (LValue (Qualifier []) [LValueComp "self" [],LValueComp "pers" [StringLit "killstreaks",IntLit 0],LValueComp "awardXp" []]))
                    , parseStatementT "maps\\mp\\gametypes\\_rank::registerScoreInfo( \"killstreak_\" + streakRef, streakPoints );" ~?= FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier ["maps","mp","gametypes","_rank"]) [LValueComp "registerScoreInfo" []])) [Binary Add (StringLit "killstreak_") (Var (LValue (Qualifier []) [LValueComp "streakRef" []])),Var (LValue (Qualifier []) [LValueComp "streakPoints" []])])
                    , parseStatementT "self waittill( \"spawned_player\" );"  ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) False (Left (LValue (Qualifier []) [LValueComp "waittill" []])) [StringLit "spawned_player"])
                    , parseStatementT "if (y>x) {println();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "println" []])) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); }" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "println" []])) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "echo" []])) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); } else {yo();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "println" []])) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "echo" []])) []))] (Just (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yo" []])) [])))
                    , parseStatementT "if (x > y) { print(); thread levelSelect(); yo(); } else if (y) {prunt();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "x" []])) (Var (LValue (Qualifier []) [LValueComp "y" []]))) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing True (Left (LValue (Qualifier []) [LValueComp "levelSelect" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yo" []])) [])]),CondStmt (Var (LValue (Qualifier []) [LValueComp "y" []])) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "prunt" []])) []))] Nothing
                    , parseStatementT "if (i == 1) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []))] Nothing
                    , parseStatementT "if (i == 1) if (j == 0) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "j" []])) (IntLit 0)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []))] Nothing)] Nothing
                    , parseStatementT "while (true) { print(); echo(); }" ~?= WhileStmt (BoolLit True) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "echo" []])) [])])
                    , parseStatementT "while (a == b && !(yolo() + grod())) {asd();}" ~?= WhileStmt (Binary BAnd (Binary Equal (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))) (BNot (Binary Add (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) []) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "grod" []])) [])))) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "asd" []])) []))
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 5) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "run" []])) [])]),CondStmt (Binary BOr (Binary Equal (IntLit 10) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "win" []])) []))] Nothing
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); default: nothingFound(); a = 6 + g(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 5) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "run" []])) [])]),CondStmt (Binary BOr (Binary Equal (IntLit 10) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "win" []])) []))] (Just (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "nothingFound" []])) []),Assign (LValue (Qualifier []) [LValueComp "a" []]) (Binary Add (IntLit 6) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "g" []])) []))]))
                    , parseStatementT "switch (yolo) { case 1: case 2: run(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 1) (Var (LValue (Qualifier []) [LValueComp "yolo" []]))) (Binary BOr (Binary Equal (IntLit 2) (Var (LValue (Qualifier []) [LValueComp "yolo" []]))) (BoolLit False))) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "run" []])) []))] Nothing
                    , parseStatementT "for (;;) print();" ~?= ForStmt Nothing (IntLit 1) Nothing (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) []))
                    , parseStatementT "for (i = 0; i < 10; i = i + 1) { doStuff(); jump(); }" ~?=  ForStmt (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0))) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (Binary Add (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)))) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "doStuff" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "jump" []])) [])])
                    , parseStatementT "for (i = 0; i < 10; i = doStuff(\"world\")) { one(); two(); }" ~?= ForStmt (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0))) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "doStuff" []])) [StringLit "world"]))) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "one" []])) []),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "two" []])) [])])
                    , parseStatementT "foreach (a, b in yolo()) {print(a + b); }" ~?= ForeachStmt ["a","b"] (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "yolo" []])) []) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) [Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))]))
                    , parseStatementT "foreach (a in dir) {print(a); run(a);}" ~?= ForeachStmt ["a"] (Var (LValue (Qualifier []) [LValueComp "dir" []])) (Seq [FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])]),FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "run" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])])])
                    , parseStatementT "foreach (a in \"name\") if (a > 10) print(a);" ~?= ForeachStmt ["a"] (StringLit "name") (CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "a" []])) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "print" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])]))] Nothing)
                    , parseStatementT "return \"adam\";" ~?= ReturnStmt (Just (StringLit "adam"))
                    , parseStatementT "break;" ~?= Break
                    , parseStatementT "continue;" ~?= Continue
                    , parseStatementT "return helloWorld();" ~?= ReturnStmt (Just (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "helloWorld" []])) []))
                    , parseStatementT "/# a = 5; #/" ~?= DebugBlock (Assign (LValue (Qualifier []) [LValueComp "a" []]) (IntLit 5))
                    , parseStatementT "yolo() { return \"yolo\"; }" ~?= FunctionDef "yolo" [] (ReturnStmt (Just (StringLit "yolo")))
                    , parseStatementT "f() {}" ~?= FunctionDef "f" [] (Seq [])
                    , parseStatementT "add(a, b) {return a + b; }" ~?= FunctionDef "add" ["a","b"] (ReturnStmt (Just (Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []])))))
                    , parseStatementT "ent [[ level.func_updatefx ]]();" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "ent" []])) False (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "func_updatefx" []]))) [])
                    , parseStatementT "P = self [[ level._pipes._pipe_methods[ type ] ]]( P, type );" ~?= Assign (LValue (Qualifier []) [LValueComp "P" []]) (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) False (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "_pipes" [],LValueComp "_pipe_methods" [Var (LValue (Qualifier []) [LValueComp "type" []])]]))) [Var (LValue (Qualifier []) [LValueComp "P" []]),Var (LValue (Qualifier []) [LValueComp "type" []])])
                    , parseStatementT "self thread [[level.perkSetFuncs[perkName]]]();" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) True (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "perkSetFuncs" [Var (LValue (Qualifier []) [LValueComp "perkName" []])]]))) [])
                    , parseStatementT "#include animscripts\\Utility;" ~?= IncludeStmt ["animscripts","Utility"]
                    , parseStatementT "#include maps\\mp\\gametypes\\_hud_util;" ~?= IncludeStmt ["maps","mp","gametypes","_hud_util"]
                    , parseStatementT "#using_animtree( \"generic_human\" );" ~?= UsingAnimTreeStmt "generic_human"
                    , parseStatementT "self UseAnimTree( #animtree );" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) False (Left (LValue (Qualifier []) [LValueComp "UseAnimTree" []])) [PreProc (Var (LValue (Qualifier []) [LValueComp "animtree" []]))])
                    , parseStatementT "precacheString( &\"ELEVATOR_CALL_HINT\" );" ~?= FunctionCallS (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "precacheString" []])) [RefStringLit "ELEVATOR_CALL_HINT"])
                    , parseStatementT "j = i++;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PostInc (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    , parseStatementT "j = --i;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PreDec (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    , parseStatementT "self.additiveTurretIdle = %saw_gunner_prone_idle_mg;" ~?= Assign (LValue (Qualifier []) [LValueComp "self" [],LValueComp "additiveTurretIdle" []]) (AnimRef (Var (LValue (Qualifier []) [LValueComp "saw_gunner_prone_idle_mg" []])))
                    , parseStatementT "waittillframeend;// let _load run first" ~?= WaittillFrameEndStmt
                    , parseStatementT "wait .05;" ~?= WaitStmt (FloatLit 0.5)
                    , parseStatementT "a = f(0)[0].a[0].a.b;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (RValueExpr (FunctionCallE Nothing False (Left (LValue (Qualifier []) [LValueComp "f" []])) [IntLit 0]) [IntLit 0] [LValueComp "a" [IntLit 0],LValueComp "a" [],LValueComp "b" []])
                    ]

main :: IO ()
main = do c <- runTestTT parseStatementTests
          print c
