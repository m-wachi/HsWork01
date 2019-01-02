import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1

insRecord :: PgHDBC.Connection -> Cal.Day -> IO Integer
insRecord conn dayVal = do
  let dn = Cal.toModifiedJulianDay dayVal
  -- dow: 1:Mon, 2:Tue ... 7:Sun
  let (_, _, dow) = CalWD.toWeekDate dayVal
  print dow
  -- let downum = CalW.fromEnum dow
  HDBC.run conn "insert into t_date values(?, ?, 0, ?)"
    [HDBC.toSql dayVal, HDBC.toSql dn, HDBC.toSql dow]



main = do
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  let a = Cal.fromGregorian 2019 1 1
  print a
  c <- insRecord conn a
  print c
  HDBC.commit conn
  res <- HDBC.quickQuery conn "select * from t_date" []
  putStrLn $ show res
  HDBC.disconnect conn


  
