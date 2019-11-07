import Test.HUnit
import LanguageStructure
import Parser


parseStatementT :: String -> Stmt
parseStatementT s = case parseStatement s of
                      (Left _)  -> error "parse error"
                      (Right e) -> e

parseStatementTests :: Test
parseStatementTests =
           TestList [ parseStatementT "a = b[1];" ~?= Assign [LValueComp "a" []] (Var [LValueComp "b" [IntLit 1]])
                    , parseStatementT "awardXp = self.pers[\"killstreaks\"][0].awardXp;" ~?= Assign [LValueComp "awardXp" []] (Var [LValueComp "self" [],LValueComp "pers" [StringLit "killstreaks",IntLit 0],LValueComp "awardXp" []])
                    , parseStatementT "maps\\mp\\gametypes\\_rank::registerScoreInfo( \"killstreak_\" + streakRef, streakPoints );" ~?= FunctionCallS (FunctionCallE Nothing False (FuncName ["maps","mp","gametypes","_rank"] "registerScoreInfo") [Binary Add (StringLit "killstreak_") (Var [LValueComp "streakRef" []]),Var [LValueComp "streakPoints" []]])
                    , parseStatementT "self waittill( \"spawned_player\" );"  ~?= FunctionCallS (FunctionCallE (Just [LValueComp "self" []]) False (FuncName [] "waittill") [StringLit "spawned_player"])
                    , parseStatementT "if (y>x) {println();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var [LValueComp "y" []]) (Var [LValueComp "x" []])) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "println") []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); }" ~?= CondStructStmt [CondStmt (Binary Greater (Var [LValueComp "y" []]) (Var [LValueComp "x" []])) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "println") [])),CondStmt (Binary Less (Binary Subtract (Var [LValueComp "y" []]) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "echo") []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); } else {yo();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var [LValueComp "y" []]) (Var [LValueComp "x" []])) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "println") [])),CondStmt (Binary Less (Binary Subtract (Var [LValueComp "y" []]) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "echo") []))] (Just (FunctionCallS (FunctionCallE Nothing False (FuncName [] "yo") [])))
                    , parseStatementT "if (x > y) { print(); thread levelSelect(); yo(); } else if (y) {prunt();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var [LValueComp "x" []]) (Var [LValueComp "y" []])) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") []),FunctionCallS (FunctionCallE Nothing True (FuncName [] "levelSelect") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "yo") [])]),CondStmt (Var [LValueComp "y" []]) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "prunt") []))] Nothing
                    , parseStatementT "while (true) { print(); echo(); }" ~?= WhileStmt (BoolConst True) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "echo") [])])
                    , parseStatementT "while (a == b && !(yolo() + grod())) {asd();}" ~?= WhileStmt (Binary BAnd (Binary Equal (Var [LValueComp "a" []]) (Var [LValueComp "b" []])) (BNot (Binary Add (FunctionCallE Nothing False (FuncName [] "yolo") []) (FunctionCallE Nothing False (FuncName [] "grod") [])))) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "asd") []))
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); }" ~?= CondStructStmt [CondStmt (Binary Equal (FunctionCallE Nothing False (FuncName [] "yolo") []) (IntLit 5)) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "run") [])]),CondStmt (Binary Equal (FunctionCallE Nothing False (FuncName [] "yolo") []) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "win") []))] Nothing
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); default: nothingFound(); a = 6 + g(); }" ~?= CondStructStmt [CondStmt (Binary Equal (FunctionCallE Nothing False (FuncName [] "yolo") []) (IntLit 5)) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "run") [])]),CondStmt (Binary Equal (FunctionCallE Nothing False (FuncName [] "yolo") []) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "win") []))] (Just (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "nothingFound") []),Assign [LValueComp "a" []] (Binary Add (IntLit 6) (FunctionCallE Nothing False (FuncName [] "g") []))]))
                    , parseStatementT "for (i = 0; i < 10; i = i + 1) { doStuff(); jump(); }" ~?= ForStmt (Assign [LValueComp "i" []] (IntLit 0)) (Binary Less (Var [LValueComp "i" []]) (IntLit 10)) (Assign [LValueComp "i" []] (Binary Add (Var [LValueComp "i" []]) (IntLit 1))) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "doStuff") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "jump") [])])
                    , parseStatementT "for (i = 0; i < 10; i = doStuff(\"world\")) { one(); two(); }" ~?= ForStmt (Assign [LValueComp "i" []] (IntLit 0)) (Binary Less (Var [LValueComp "i" []]) (IntLit 10)) (Assign [LValueComp "i" []] (FunctionCallE Nothing False (FuncName [] "doStuff") [StringLit "world"])) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "one") []),FunctionCallS (FunctionCallE Nothing False (FuncName [] "two") [])])
                    , parseStatementT "foreach (a, b in yolo()) {print(a + b); }" ~?= ForeachStmt ["a","b"] (FunctionCallE Nothing False (FuncName [] "yolo") []) (FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") [Binary Add (Var [LValueComp "a" []]) (Var [LValueComp "b" []])]))
                    , parseStatementT "foreach (a in dir) {print(a); run(a);}" ~?= ForeachStmt ["a"] (Var [LValueComp "dir" []]) (Seq [FunctionCallS (FunctionCallE Nothing False (FuncName [] "print") [Var [LValueComp "a" []]]),FunctionCallS (FunctionCallE Nothing False (FuncName [] "run") [Var [LValueComp "a" []]])])
                    ]

main :: IO ()
main = do c <- runTestTT parseStatementTests
          print c
