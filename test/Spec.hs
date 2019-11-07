import Test.HUnit
import Lib


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
                    ]

main :: IO ()
main = do c <- runTestTT parseStatementTests
          print c
