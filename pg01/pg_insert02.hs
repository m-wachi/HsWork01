import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal

main = do
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  let a = Cal.fromGregorian 2018 12 3
  print a
  let b = HDBC.toSql a
  print b
  HDBC.run conn "insert into tbl02 values(?, ?)" [HDBC.toSql (3::Int), b]
  HDBC.commit conn
  res <- HDBC.quickQuery conn "select * from tbl02" []
  putStrLn $ show res
  HDBC.disconnect conn


  
