import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1

import qualified MyModule01 as MyM1


{-
getSalesNum :: [String] -> SalesNum
getSalesNum [sDeliveryDate, sCustomerCode, sStoreCd, sMercCd, sQuantity] =
  SalesNum (read sDeliveryDate) (read sCustomerCode) (read sStoreCd) (read sMercCd) (read sQuantity)

                                
insTSalesNum :: PgHDBC.Connection -> SalesNum -> IO Integer
insTSalesNum conn salesNum = do
  let sqlDeliveryDate = HDBC.toSql $ snDeliveryDate salesNum
  let sqlCustomerCd = HDBC.toSql $ snCustomerCode salesNum
  let sqlStoreCd = HDBC.toSql $ snStoreCd salesNum
  let sqlMercCd = HDBC.toSql $ snMercCd salesNum
  let sqlQuantity = HDBC.toSql $ snQuantity salesNum
  HDBC.run conn "insert into wk_sales_num01 values(?, ?, ?, ?, ?)"
    [sqlDeliveryDate, sqlCustomerCd, sqlStoreCd, sqlMercCd, sqlQuantity]
-}
-- genDayList :: Cal.Day -> Cal.Day -> [Cal.Day]
-- genDayList from to =
--   takeWhile (<= to) $ iterate succ from
{-
splitComma :: String -> [String]
splitComma "" = [""]
splitComma s
  | b == "" = [a]
  | otherwise = a : (splitComma $ tail b)
  where
    (a, b) = break (== ',') s

remove2Quote :: String -> String
remove2Quote cs = 
    if '"' == (last cs2) then init cs2 else cs2
  where
    cs2 = if '"' == head cs then tail cs else cs
-}

main = do
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  res <- HDBC.quickQuery conn (
    "select " ++
    "    sn1.delivery_date_n, sn1.customer_cd, " ++
    "    sn1.store_cd, sn1.merc_cd, sn1.quantity, " ++
    "    dt.date_num, dt.week_num, dt.day_of_week " ++
    "from t_sales_num01 sn1" ++
    "    inner join t_date dt " ++
    "        on sn1.delivery_date_n = dt.date_n8 " ++
    "where merc_cd=105617 and customer_cd=941 " ++
    "order by sn1.customer_cd, sn1.store_cd, sn1.merc_cd, sn1.delivery_date_n ") []
  --print res
  let r1 = head res
  let r2 = head $ tail res
  print "---"
  print r1
  print r2
  print "---"
  let tpl1 = MyM1.fromSql2Tpl r1
  --let tpl2 = MyM1.fromSql2Tpl r2
  print tpl1
  --print tpl2
  print "---"
  let snw1 = MyM1.fromTpl2SNW tpl1
  let snw2 = MyM1.fromSql2SNW r2
  print snw1
  print snw2
  print "---"
  print (MyM1.isSameGroup01 snw1 snw2)
  print "---"
  let res20 = take 20 res
  print res20
  print "---"
  let snws20 = map MyM1.fromSql2SNW res20
  print snws20
  let (pa1, pa2) = span (MyM1.isSameGroup01 snw1) snws20
  print pa1
  print "---"
  let snwgs = MyM1.splitGroup01 [] snws20
  let snwgs1 = head snwgs
  let snwgs2 = head $ tail snwgs
  print snwgs1
  print snwgs2
  print "---"
  print $ MyM1.mergeSNW snw1 snw2
  print "---"
  print $ MyM1.accumSNW snwgs1
  print $ MyM1.accumSNW snwgs2
  print "---"
  let snws2 = map MyM1.accumSNW snwgs
  print snws2
  HDBC.disconnect conn

  {-
  fileContents <- readFile "result2.csv"
  let lineData = tail $ lines fileContents
  let f1 = (map remove2Quote) . splitComma
  let lineData2 = map f1 lineData
  -- print lineData2
  let salesNums = map getSalesNum $ lineData2
  -- print "---"
  --print salesNums
  -}
  -- insRecord conn sn1
  --d <- mapM (insTSalesNum conn) salesNums -- use mapM if function is Action(IO Monad)
  -- HDBC.commit conn

  
