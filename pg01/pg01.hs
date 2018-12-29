import Database.HDBC
import Database.HDBC.PostgreSQL

main = do
  conn <- connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  res <- quickQuery conn "select * from tbl01" []
  putStrLn $ show res


  
