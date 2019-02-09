import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1

insRecord :: PgHDBC.Connection -> Cal.Day -> IO Integer
insRecord conn dayVal = do
  let n8 = convDay2N8 dayVal
  let dn = Cal.toModifiedJulianDay dayVal
  -- dow: 1:Mon, 2:Tue ... 7:Sun
  let (_, _, dow) = CalWD.toWeekDate dayVal
  -- print dow
  let tmp = (dn - 4 - (fromIntegral dow)) :: Integer
  let wn = tmp `div` (7::Integer) 
  -- print wn
  -- let downum = CalW.fromEnum dow
  HDBC.run conn "insert into t_date values(?, ?, ?, ?, ?)"
    [(HDBC.toSql n8), (HDBC.toSql dayVal), (HDBC.toSql dn), (HDBC.toSql wn), (HDBC.toSql dow)]


genDayList :: Cal.Day -> Cal.Day -> [Cal.Day]
genDayList from to =
  takeWhile (<= to) $ iterate succ from

convDay2N8 :: Cal.Day -> Int
convDay2N8 dayVal =
    ((fromIntegral y) * 10000) + m * 100 + d
  where
    (y, m, d) = Cal.toGregorian dayVal
   

main = do
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  let a = Cal.fromGregorian 2016 1 1
  print a
  let b = Cal.fromGregorian 2019 12 31
  print b
  let c = genDayList a b
  -- print c
  --print (convDay2N8 b)
  let f1 = insRecord conn
  d <- mapM (insRecord conn) c -- use mapM if function is Action(IO Monad)
  -- print d
  HDBC.commit conn
  -- res <- HDBC.quickQuery conn "select * from t_date" []
  -- putStrLn $ show res
  HDBC.disconnect conn


  
