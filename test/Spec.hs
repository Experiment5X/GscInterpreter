import Test.HUnit
import LanguageStructure
import Parser
import Interpreter
import qualified Data.Map as Map


parseStatementT :: String -> Stmt
parseStatementT s = case parseStatement s of
                      (Left _)  -> error ("parse error: " ++ s)
                      (Right e) -> e

parseExprT :: String -> Expr
parseExprT s = case parseExpression s of
                 (Left _)  -> error ("parse error: " ++ s)
                 (Right e) -> e

evalExprT :: Expr -> Value
evalExprT e = case evalExpr2 e emptyEnv of
                (Left _)  -> error ("eval error: " ++ show e)
                (Right v) -> v
                
evalStmtT :: Stmt -> GscEnv
evalStmtT stmt = case evalStmt2 stmt emptyEnv of
                   (Left _)  -> error ("eval error: " ++ show stmt)
                   (Right v) -> v

parseStatementTests :: Test
parseStatementTests =
           TestList [ parseStatementT "a = b[1];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Var (LValue (Qualifier []) [LValueComp "b" [IntLit 1]]))
                    , parseStatementT "a = 0.5;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (FloatLit 0.5)
                    , parseStatementT "a = true;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (BoolLit True)
                    , parseStatementT "a = [];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [])
                    , parseStatementT "a = [1,2,3];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [IntLit 1,IntLit 2,IntLit 3])
                    , parseStatementT "a = [f(), 1, 0.3, \"adam\"];" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (ListLit [FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "f" []])) [],IntLit 1,FloatLit 0.3,StringLit "adam"])
                    , parseStatementT "a = (1,2,3);" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Vec3Lit (IntLit 1) (IntLit 2) (IntLit 3))
                    , parseStatementT "a += f();" ~?= PlusEquals (LValue (Qualifier []) [LValueComp "a" []]) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "f" []])) [])
                    , parseStatementT "--index;" ~?= AssignExprStmt (PreDec (Var (LValue (Qualifier []) [LValueComp "index" []])))
                    , parseStatementT "yo = other\\module::var;" ~?= Assign (LValue (Qualifier []) [LValueComp "yo" []]) (Var (LValue (Qualifier ["other","module"]) [LValueComp "var" []]))
                    , parseStatementT " a = ::yolo;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (FuncReference Nothing "yolo")
                    , parseStatementT "awardXp = self.pers[\"killstreaks\"][0].awardXp;" ~?= Assign (LValue (Qualifier []) [LValueComp "awardXp" []]) (Var (LValue (Qualifier []) [LValueComp "self" [],LValueComp "pers" [StringLit "killstreaks",IntLit 0],LValueComp "awardXp" []]))
                    , parseStatementT "maps\\mp\\gametypes\\_rank::registerScoreInfo( \"killstreak_\" + streakRef, streakPoints );" ~?= FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier ["maps","mp","gametypes","_rank"]) [LValueComp "registerScoreInfo" []])) [Binary Add (StringLit "killstreak_") (Var (LValue (Qualifier []) [LValueComp "streakRef" []])),Var (LValue (Qualifier []) [LValueComp "streakPoints" []])])
                    , parseStatementT "self waittill( \"spawned_player\" );"  ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) Default (Left (LValue (Qualifier []) [LValueComp "waittill" []])) [StringLit "spawned_player"])
                    , parseStatementT "if (y>x) {println();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "println" []])) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); }" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "println" []])) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "echo" []])) []))] Nothing
                    , parseStatementT "if (y>x) {println();} else if (y - 6 < 1) { echo(); } else {yo();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "y" []])) (Var (LValue (Qualifier []) [LValueComp "x" []]))) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "println" []])) [])),CondStmt (Binary Less (Binary Subtract (Var (LValue (Qualifier []) [LValueComp "y" []])) (IntLit 6)) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "echo" []])) []))] (Just (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yo" []])) [])))
                    , parseStatementT "if (x > y) { print(); thread levelSelect(); yo(); } else if (y) {prunt();}" ~?= CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "x" []])) (Var (LValue (Qualifier []) [LValueComp "y" []]))) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing Thread (Left (LValue (Qualifier []) [LValueComp "levelSelect" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yo" []])) [])]),CondStmt (Var (LValue (Qualifier []) [LValueComp "y" []])) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "prunt" []])) []))] Nothing
                    , parseStatementT "if (i == 1) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []))] Nothing
                    , parseStatementT "if (i == 1) if (j == 0) print();" ~?= CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)) (CondStructStmt [CondStmt (Binary Equal (Var (LValue (Qualifier []) [LValueComp "j" []])) (IntLit 0)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []))] Nothing)] Nothing
                    , parseStatementT "while (true) { print(); echo(); }" ~?= WhileStmt (BoolLit True) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "echo" []])) [])])
                    , parseStatementT "while (a == b && !(yolo() + grod())) {asd();}" ~?= WhileStmt (Binary BAnd (Binary Equal (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))) (BNot (Binary Add (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) []) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "grod" []])) [])))) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "asd" []])) []))
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 5) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "run" []])) [])]),CondStmt (Binary BOr (Binary Equal (IntLit 10) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "win" []])) []))] Nothing
                    , parseStatementT "switch (yolo()) { case 5: print(); run(); case 10: win(); default: nothingFound(); a = 6 + g(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 5) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "run" []])) [])]),CondStmt (Binary BOr (Binary Equal (IntLit 10) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) [])) (BoolLit False)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "win" []])) []))] (Just (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "nothingFound" []])) []),Assign (LValue (Qualifier []) [LValueComp "a" []]) (Binary Add (IntLit 6) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "g" []])) []))]))
                    , parseStatementT "switch (yolo) { case 1: case 2: run(); }" ~?= CondStructStmt [CondStmt (Binary BOr (Binary Equal (IntLit 1) (Var (LValue (Qualifier []) [LValueComp "yolo" []]))) (Binary BOr (Binary Equal (IntLit 2) (Var (LValue (Qualifier []) [LValueComp "yolo" []]))) (BoolLit False))) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "run" []])) []))] Nothing
                    , parseStatementT "for (;;) print();" ~?= ForStmt Nothing (IntLit 1) Nothing (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) []))
                    , parseStatementT "for (i = 0; i < 10; i = i + 1) { doStuff(); jump(); }" ~?=  ForStmt (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0))) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (Binary Add (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 1)))) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "doStuff" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "jump" []])) [])])
                    , parseStatementT "for (i = 0; i < 10; i = doStuff(\"world\")) { one(); two(); }" ~?= ForStmt (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 0))) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 10)) (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "doStuff" []])) [StringLit "world"]))) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "one" []])) []),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "two" []])) [])])
                    , parseStatementT "foreach (a, b in yolo()) {print(a + b); }" ~?= ForeachStmt ["a","b"] (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "yolo" []])) []) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) [Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))]))
                    , parseStatementT "foreach (a in dir) {print(a); run(a);}" ~?= ForeachStmt ["a"] (Var (LValue (Qualifier []) [LValueComp "dir" []])) (Seq [FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])]),FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "run" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])])])
                    , parseStatementT "foreach (a in \"name\") if (a > 10) print(a);" ~?= ForeachStmt ["a"] (StringLit "name") (CondStructStmt [CondStmt (Binary Greater (Var (LValue (Qualifier []) [LValueComp "a" []])) (IntLit 10)) (FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "print" []])) [Var (LValue (Qualifier []) [LValueComp "a" []])]))] Nothing)
                    , parseStatementT "return \"adam\";" ~?= ReturnStmt (Just (StringLit "adam"))
                    , parseStatementT "break;" ~?= Break
                    , parseStatementT "continue;" ~?= Continue
                    , parseStatementT "return helloWorld();" ~?= ReturnStmt (Just (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "helloWorld" []])) []))
                    , parseStatementT "/# a = 5; #/" ~?= DebugBlock (Assign (LValue (Qualifier []) [LValueComp "a" []]) (IntLit 5))
                    , parseStatementT "yolo() { return \"yolo\"; }" ~?= FunctionDef "yolo" [] (ReturnStmt (Just (StringLit "yolo")))
                    , parseStatementT "f() {}" ~?= FunctionDef "f" [] (Seq [])
                    , parseStatementT "add(a, b) {return a + b; }" ~?= FunctionDef "add" ["a","b"] (ReturnStmt (Just (Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []])))))
                    , parseStatementT "ent [[ level.func_updatefx ]]();" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "ent" []])) Default (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "func_updatefx" []]))) [])
                    , parseStatementT "P = self [[ level._pipes._pipe_methods[ type ] ]]( P, type );" ~?= Assign (LValue (Qualifier []) [LValueComp "P" []]) (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) Default (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "_pipes" [],LValueComp "_pipe_methods" [Var (LValue (Qualifier []) [LValueComp "type" []])]]))) [Var (LValue (Qualifier []) [LValueComp "P" []]),Var (LValue (Qualifier []) [LValueComp "type" []])])
                    , parseStatementT "self thread [[level.perkSetFuncs[perkName]]]();" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) Thread (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "perkSetFuncs" [Var (LValue (Qualifier []) [LValueComp "perkName" []])]]))) [])
                    , parseStatementT "self.model call [[ level.connectPathsFunction ]]();" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" [],LValueComp "model" []])) Call (Right (FuncDereference (LValue (Qualifier []) [LValueComp "level" [],LValueComp "connectPathsFunction" []]))) [])
                    , parseStatementT "#include animscripts\\Utility;" ~?= IncludeStmt ["animscripts","Utility"]
                    , parseStatementT "#include maps\\mp\\gametypes\\_hud_util;" ~?= IncludeStmt ["maps","mp","gametypes","_hud_util"]
                    , parseStatementT "#using_animtree( \"generic_human\" );" ~?= UsingAnimTreeStmt "generic_human"
                    , parseStatementT "self UseAnimTree( #animtree );" ~?= FunctionCallS (FunctionCallE (Just (LValue (Qualifier []) [LValueComp "self" []])) Default (Left (LValue (Qualifier []) [LValueComp "UseAnimTree" []])) [PreProc (Var (LValue (Qualifier []) [LValueComp "animtree" []]))])
                    , parseStatementT "precacheString( &\"ELEVATOR_CALL_HINT\" );" ~?= FunctionCallS (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "precacheString" []])) [RefStringLit "ELEVATOR_CALL_HINT"])
                    , parseStatementT "j = i++;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PostInc (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    , parseStatementT "j = --i;" ~?= Assign (LValue (Qualifier []) [LValueComp "j" []]) (PreDec (Var (LValue (Qualifier []) [LValueComp "i" []])))
                    , parseStatementT "self.additiveTurretIdle = %saw_gunner_prone_idle_mg;" ~?= Assign (LValue (Qualifier []) [LValueComp "self" [],LValueComp "additiveTurretIdle" []]) (AnimRef (Var (LValue (Qualifier []) [LValueComp "saw_gunner_prone_idle_mg" []])))
                    , parseStatementT "waittillframeend;// let _load run first" ~?= WaittillFrameEndStmt
                    , parseStatementT "wait .05;" ~?= WaitStmt (FloatLit 0.5)
                    , parseStatementT "a = f(0)[0].a[0].a.b;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (RValueExpr (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "f" []])) [IntLit 0]) [IntLit 0] [LValueComp "a" [IntLit 0],LValueComp "a" [],LValueComp "b" []])
                    , parseStatementT "a = 3*.5;" ~?= Assign (LValue (Qualifier []) [LValueComp "a" []]) (Binary Multiply (IntLit 3) (FloatLit 0.5))
                    , parseStatementT "/# #/" ~?= DebugBlock (Seq [])
                    , parseStatementT ";;;" ~?= Seq [Seq [],Seq [],Seq []]
                    , parseStatementT "childthread noself_delayCall( delay );" ~?= FunctionCallS (FunctionCallE Nothing ChildThread (Left (LValue (Qualifier []) [LValueComp "noself_delayCall" []])) [Var (LValue (Qualifier []) [LValueComp "delay" []])])
                    ]
                    
evalExprTests :: Test
evalExprTests = 
     TestList [ evalExprT (parseExprT "3 + 6 * 10") ~?= VInt 63
              , evalExprT (parseExprT "5 < 6") ~?= VBool True
              , evalExprT (parseExprT "5 == 6") ~?= VBool False
              , evalExprT (parseExprT "5 == 5") ~?= VBool True
              , evalExprT (parseExprT "5.6 > 5") ~?= VBool True
              , evalExprT (parseExprT "\"hello \" + \"world\"") ~?= VString "hello world"
              ]
              
evalStmtTests :: Test
evalStmtTests = 
     TestList [ Map.toList (evalStmtT (parseStatementT "a = 5; b = a * 2;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VInt 5),("b",VInt 10)]
              , Map.toList (evalStmtT (parseStatementT "if (5 == 6) { a = \"bad\"; } else { a= \"good\"; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VString "good")]
              , Map.toList (evalStmtT (parseStatementT "switch (9) { case 8: a = 0; break; case 9: a = 1; break; default: a = 2; break; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VInt 1)]
              , Map.toList (evalStmtT (parseStatementT "a = [1,2,3]; b = a; a[0] = 20; if (b[0] == 20) c = \"success\";")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VInt 0,VInt 20),(VInt 1,VInt 2),(VInt 2,VInt 3)])])),("a",VRef 0),("b",VRef 0),("c",VString "success")]
              , Map.toList (evalStmtT (parseStatementT "a = []; a[\"name\"] = \"adam\"; b = a.name;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "name",VString "adam")])])),("a",VRef 0),("b",VString "adam")]
              , Map.toList (evalStmtT (parseStatementT "addr = []; addr[\"street\"] = \"123 Main\"; person = []; person[\"address\"] = addr; street = person.address.street;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 2),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "street",VString "123 Main")]),(1,Map.fromList [(VString "address",VRef 0)])])),("addr",VRef 0),("person",VRef 1),("street",VString "123 Main")]
              , Map.toList (evalStmtT (parseStatementT "person = []; person.name = \"Adam\"; person.address = []; person.address.street = \"Main Street\"; street = person.address.street;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 2),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "address",VRef 1),(VString "name",VString "Adam")]),(1,Map.fromList [(VString "street",VString "Main Street")])])),("person",VRef 0),("street",VString "Main Street")]
              , Map.toList (evalStmtT (parseStatementT "a = 0; b = 1; while (a < 10) { b *= 2; a++; } ")) ~?=  [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VInt 10),("b",VInt 1024)]
              , Map.toList (evalStmtT (parseStatementT "n = 1; for (i = 0; i < 10; i++) { n *= 2; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("i",VInt 10),("n",VInt 1024)]
              , Map.toList (evalStmtT (parseStatementT "i = 0; n = 1; while (i < 10) { n *= 2; i++; if (n > 16) break; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("i",VInt 5),("n",VInt 32)]
              , Map.toList (evalStmtT (parseStatementT "i = 0; j = 0; n = 1; while (i < 10) { i++; while (j < 10) { if (j > 3) break; j++; n++; } j = 0; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("i",VInt 10),("j",VInt 0),("n",VInt 41)]
              , Map.toList (evalStmtT (parseStatementT "sum = 0; for (i = 0; i < 10; i++) { if (sum > 20) break; sum += i; }")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("i",VInt 7),("sum",VInt 21)]
              , Map.toList (evalStmtT (parseStatementT "myAdd(a, b) { return a + b; } ; c = myAdd(4, 6);")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [("myAdd",VFunctionDef ["a","b"] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList []))]) (ReturnStmt (Just (Binary Add (Var (LValue (Qualifier []) [LValueComp "a" []])) (Var (LValue (Qualifier []) [LValueComp "b" []]))))))])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("c",VInt 10)]
              , Map.toList (evalStmtT (parseStatementT "setAge(p, age) { p.age = age; return; } ; person = []; setAge(person, 24); a = person.age;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [("setAge",VFunctionDef ["p","age"] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList []))]) (Seq [Assign (LValue (Qualifier []) [LValueComp "p" [],LValueComp "age" []]) (Var (LValue (Qualifier []) [LValueComp "age" []])),ReturnStmt Nothing]))])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "age",VInt 24)])])),("a",VInt 24),("person",VRef 0)]
              , Map.toList (evalStmtT (parseStatementT "getFirstEven() { for (i = 1; i < 20; i++) if (i % 2 == 0) return i; } a = getFirstEven();")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [("getFirstEven",VFunctionDef [] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList []))]) (ForStmt (Just (Assign (LValue (Qualifier []) [LValueComp "i" []]) (IntLit 1))) (Binary Less (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 20)) (Just (AssignExprStmt (PostInc (Var (LValue (Qualifier []) [LValueComp "i" []]))))) (CondStructStmt [CondStmt (Binary Equal (Binary Modulus (Var (LValue (Qualifier []) [LValueComp "i" []])) (IntLit 2)) (IntLit 0)) (ReturnStmt (Just (Var (LValue (Qualifier []) [LValueComp "i" []]))))] Nothing)))])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VInt 2)]
              , Map.toList (evalStmtT (parseStatementT "f(x) { g (y) { return x + y; } return g(5); } a = f(2);")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [("f",VFunctionDef ["x"] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList []))]) (Seq [FunctionDef "g" ["y"] (ReturnStmt (Just (Binary Add (Var (LValue (Qualifier []) [LValueComp "x" []])) (Var (LValue (Qualifier []) [LValueComp "y" []]))))),ReturnStmt (Just (FunctionCallE Nothing Default (Left (LValue (Qualifier []) [LValueComp "g" []])) [IntLit 5]))]))])),("*nextObjId",VInt 0),("*objects",VStore (Map.fromList [])),("a",VInt 7)]
              , Map.toList (evalStmtT (parseStatementT "person = []; person.age = 24; canDrink() { return self.age >= 21; } person.canDrink = ::canDrink; legal = person canDrink();")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [("canDrink",VFunctionDef [] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "age",VInt 24)])])),("person",VRef 0)]) (ReturnStmt (Just (Binary GreaterEq (Var (LValue (Qualifier []) [LValueComp "self" [],LValueComp "age" []])) (IntLit 21)))))])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "age",VInt 24),(VString "canDrink",VFunctionDef [] (Map.fromList [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "age",VInt 24)])])),("person",VRef 0)]) (ReturnStmt (Just (Binary GreaterEq (Var (LValue (Qualifier []) [LValueComp "self" [],LValueComp "age" []])) (IntLit 21)))))])])),("legal",VBool True),("person",VRef 0)]
              , Map.toList (evalStmtT (parseStatementT "a = [1,2,3,4];b = a.size;a.test = \"another\";c = a.size;")) ~?= [("*functionDefs",VFunctionDefs (Map.fromList [])),("*nextObjId",VInt 1),("*objects",VStore (Map.fromList [(0,Map.fromList [(VString "test",VString "another"),(VInt 0,VInt 1),(VInt 1,VInt 2),(VInt 2,VInt 3),(VInt 3,VInt 4)])])),("a",VRef 0),("b",VInt 4),("c",VInt 5)]
              ]

main :: IO ()
main = do putStrLn "Parsing Tests"
          c1 <- runTestTT parseStatementTests
          print c1
          
          putStrLn ""
          
          putStrLn "Eval Expression Tests"
          c2 <- runTestTT evalExprTests
          print c2
          
          putStrLn ""
          
          putStrLn "Eval Statement Tests"
          c3 <- runTestTT evalStmtTests
          print c3
