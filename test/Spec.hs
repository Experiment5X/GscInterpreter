import Test.HUnit
import LanguageStructure
import Parser


parseStatementT :: String -> Stmt
parseStatementT s = case parseStatement s of
                      (Left _)  -> error "parse error"
                      (Right e) -> e

parseStatementTests :: Test
parseStatementTests =
           TestList [ parseStatementT "a = b[1];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Var (LValue (Qualifier []) [LValueComp "b" [IntLit 1]]))
                    , parseStatementT "yo = other\\module::var;" ~?= Assign (LValue (Qualifier []) [LValueComp "yo" []]) (Var (LValue (Qualifier ["other","module"]) [LValueComp "var" []]))
                    , parseStatementT "awardXp = self.pers[\"killstreaks\"][0].awardXp;" ~?= Assign (LValue (Qualifier []) [LValueComp "awardXp" []]) (Var (LValue (Qualifier []) [LValueComp "self" [],LValueComp "pers" [StringLit "killstreaks",IntLit 0],LValueComp "awardXp" []]))
                    , parseStatementT "maps\\mp\\gametypes\\_rank::registerScoreInfo( \"killstreak_\" + streakRef, streakPoints );" ~?= FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier ["maps","mp","gametypes","_rank"]) [LValueComp "registerScoreInfo" []]) [Binary Add (StringLit "killstreak_") (Var (LValue (Qualifier []) [LValueComp "streakRef" []])),Var (LValue (Qualifier []) [LValueComp "streakPoints" []])])
                    , parseStatementT "self waittill( \"spawned_player\" );"  ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) False (LValue (Qualifier []) [LValueComp "waittill" []]) [StringLit "spawned_player"])
                    , parseStatementT "if (y>x) {println();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "println" []]) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); }" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "println" []]) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "echo" []]) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); } else {yo();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "println" []]) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "echo" []]) []))] (Just (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yo" []]) [])))
                    , parseStatementT "if (x > y) { print(); thread levelSelect(); yo(); } else if (y) {prunt();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "x" []])) (Var (LValue (Qualifier []) [LValueComp "y" []]))) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []),FunctionCallS (FunctionCallE Nothing True (LValue (Qualifier []) [LValueComp "levelSelect" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yo" []]) [])]),CondStmt (Var (LValue (Qualifier []) [LValueComp "y" []])) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "prunt" []]) []))] Nothing
                    , parseStatementT "if (i == 1) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []))] Nothing
                    , parseStatementT "if (i == 1) if (j == 0) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "j" []])) (IntLit 0)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []))] Nothing)] Nothing
                    , parseStatementT "while (true) { print(); echo(); }" ~?= WhileStmt (BoolConst True) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "echo" []]) [])])
                    , parseStatementT "while (a == b && !(yolo() + grod())) {asd();}" ~?= WhileStmt (Binary BAnd (Binary Equal (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))) (BNot (Binary Add (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "grod" []]) [])))) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "asd" []]) []))
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); }" ~?= CondStructStmt [CondStmt (Binary Equal (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (IntLit 5)) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "run" []]) [])]),CondStmt (Binary Equal (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "win" []]) []))] Nothing
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); default: nothingFound(); a = 6 + g(); }" ~?= CondStructStmt [CondStmt (Binary Equal (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (IntLit 5)) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "run" []]) [])]),CondStmt (Binary Equal (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "win" []]) []))] (Just (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "nothingFound" []]) []),Assign (LValue (Qualifier []) [LValueComp "a" []]) (Binary Add (IntLit 6) (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "g" []]) []))]))
                    , parseStatementT "for (i = 0; i < 10; i = i + 1) { doStuff(); jump(); }" ~?=  ForStmt (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0)) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Assign (LValue (Qualifier []) [LValueComp "i" []]) (Binary Add (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1))) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "doStuff" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "jump" []]) [])])
                    , parseStatementT "for (i = 0; i < 10; i = doStuff(\"world\")) { one(); two(); }" ~?= ForStmt (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0)) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Assign (LValue (Qualifier []) [LValueComp "i" []]) (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "doStuff" []]) [StringLit "world"])) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "one" []]) []),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "two" []]) [])])
                    , parseStatementT "foreach (a, b in yolo()) {print(a + b); }" ~?= ForeachStmt ["a","b"] (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "yolo" []]) []) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) [Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))]))
                    , parseStatementT "foreach (a in dir) {print(a); run(a);}" ~?= ForeachStmt ["a"] (Var (LValue (Qualifier []) [LValueComp "dir" []])) (Seq [FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) [Var (LValue (Qualifier []) [LValueComp "a" []])]),FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "run" []]) [Var (LValue (Qualifier []) [LValueComp "a" []])])])
                    , parseStatementT "foreach (a in \"name\") if (a > 10) print(a);" ~?= ForeachStmt ["a"] (StringLit "name") (CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "a" []])) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "print" []]) [Var (LValue (Qualifier []) [LValueComp "a" []])]))] Nothing)
                    , parseStatementT "return \"adam\";" ~?= ReturnStmt (StringLit "adam")
                    , parseStatementT "return helloWorld();" ~?= ReturnStmt (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "helloWorld" []]) [])
                    , parseStatementT "yolo() { return \"yolo\"; }" ~?= FunctionDef "yolo" [] (ReturnStmt (StringLit "yolo"))
                    , parseStatementT "add(a, b) {return a + b; }" ~?= FunctionDef "add" ["a","b"] (ReturnStmt (Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))))
                    , parseStatementT "#include animscripts\\Utility;" ~?= IncludeStmt ["animscripts","Utility"]
                    , parseStatementT "#include maps\\mp\\gametypes\\_hud_util;" ~?= IncludeStmt ["maps","mp","gametypes","_hud_util"]
                    , parseStatementT "#using_animtree( \"generic_human\" );" ~?= UsingAnimTreeStmt "generic_human"
                    , parseStatementT "self UseAnimTree( #animtree );" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) False (LValue (Qualifier []) [LValueComp "UseAnimTree" []]) [PreProc (Var (LValue (Qualifier []) [LValueComp "animtree" []]))])
                    , parseStatementT "precacheString( &\"ELEVATOR_CALL_HINT\" );" ~?= FunctionCallS (FunctionCallE Nothing False (LValue (Qualifier []) [LValueComp "precacheString" []]) [RefStringLit "ELEVATOR_CALL_HINT"])
                    , parseStatementT "j = i++;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PostInc (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    , parseStatementT "j = --i;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PreDec (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    ]

main :: IO ()
main = do c <- runTestTT parseStatementTests
          print c
