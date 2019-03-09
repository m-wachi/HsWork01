import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1
import qualified System.Environment as SysEnv
import qualified System.Exit as SysExit
import qualified MyModule01 as MyM1


batchInsTSNW :: PgHDBC.Connection -> [MyM1.SalesNumWeek] -> IO Integer
batchInsTSNW conn snws = do
  let snwsTake = take 500 snws
  let snwsLeft = drop 500 snws
  if 0 == (length snwsTake) then return 0
  else do
    d <- mapM (MyM1.insTSalesNumWeek conn) snwsTake -- use mapM if function is Action(IO Monad)
    -- print $ sum d
    let cnt = sum d
    putStr "."
    HDBC.commit conn
    cnt2 <- batchInsTSNW conn snwsLeft
    return $ cnt + cnt2

{-
コマンドライン引数処理
-}
handleArgs :: IO (Cal.Day, Cal.Day)
handleArgs = do
  args <- SysEnv.getArgs
  if 2 > (length args)
    then do
      print "usage: ins_sales_num02 fromYYYYMMDD toYYYYMMDD"
      SysExit.exitWith $ SysExit.ExitFailure 9
    else return ()
  let dtFrom = MyM1.convN8ToDay $ read $ head args :: Cal.Day
  let dtTo = MyM1.convN8ToDay $ read $ args !! 1 :: Cal.Day
  print (dtFrom, dtTo) 
  let dtFrom2 = MyM1.getThisMonday dtFrom
  let dtTo2 = MyM1.getNextSunday dtTo
  if dtFrom2 /= dtFrom || dtTo2 /= dtTo
    then print $ "target period start: " ++ (show dtFrom) ++ "->" ++ (show dtFrom2) ++ ", end: " ++ (show dtTo) ++ "->" ++ (show dtTo2)
    else print $ "target period " ++ (show dtFrom2) ++ ", " ++ (show dtTo2)
  return (dtFrom2, dtTo2)

selInsOneWeek :: PgHDBC.Connection -> Cal.Day -> IO Integer
selInsOneWeek conn dtMonday = do
  res <- MyM1.getSalesNumOneWeek conn dtMonday
  
  print $ (show dtMonday) ++ ", count=" ++ (show $ length res)
  let snws = map MyM1.fromSql2SNW res
  let snwgs = MyM1.splitGroup01 [] snws
  let snws2 = map MyM1.accumSNW snwgs
  cnt <- batchInsTSNW conn snws2
  putStrLn ""
  return cnt

main = do
  (dtFrom, dtTo) <- handleArgs
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  let dtMondays = MyM1.genMondayList dtFrom dtTo
  print dtMondays
  d <- mapM (selInsOneWeek conn) dtMondays -- use mapM if function is Action(IO Monad)
  --SysExit.exitSuccess
  -- res <- MyM1.getSalesNumFromDb conn 20170101 20170110
  -- res <- MyM1.getSalesNumOneWeek conn dtFrom
  -- print $ "count=" ++ (show $ length res)
  -- print "---"
  -- let res20 = take 20 res
  -- -- print res20
  -- -- print "---"
  -- let snws = map MyM1.fromSql2SNW res
  -- let snwgs = MyM1.splitGroup01 [] snws
  -- let snws2 = map MyM1.accumSNW snwgs
  -- -- print snws2
  -- print "---"
  -- --d <- mapM (MyM1.insTSalesNumWeek conn) snws2 -- use mapM if function is Action(IO Monad)
  -- --print $ sum d
  -- -- HDBC.commit conn
  -- batchInsTSNW conn snws2
  HDBC.disconnect conn

  
