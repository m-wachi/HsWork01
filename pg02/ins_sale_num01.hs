import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1

data SalesNum = SalesNum {
  snDeliveryDate :: Int,
  snCustomerCode :: Int,
  snStoreCd :: Int,
  snMercCd :: Int,
  snQuantity :: Int
  } deriving (Show)


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

-- genDayList :: Cal.Day -> Cal.Day -> [Cal.Day]
-- genDayList from to =
--   takeWhile (<= to) $ iterate succ from

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

main = do
  fileContents <- readFile "result2.csv"
  let lineData = tail $ lines fileContents
  let f1 = (map remove2Quote) . splitComma
  let lineData2 = map f1 lineData
  -- print lineData2
  let salesNums = map getSalesNum $ lineData2
  -- print "---"
  --print salesNums
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  -- insRecord conn sn1
  d <- mapM (insTSalesNum conn) salesNums -- use mapM if function is Action(IO Monad)
  HDBC.commit conn
  -- res <- HDBC.quickQuery conn "select * from t_sales_num01" []
  -- putStrLn $ show res
  HDBC.disconnect conn


  
