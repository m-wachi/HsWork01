import Database.HDBC
import Database.HDBC.PostgreSQL

main = do
  conn <- connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  run conn "insert into tbl01 values(?, ?)" [toSql (5::Int), toSql "heyho"]
  commit conn
  res <- quickQuery conn "select * from tbl01" []
  putStrLn $ show res
  disconnect conn



  
