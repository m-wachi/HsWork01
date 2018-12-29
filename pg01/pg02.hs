import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.Calendar

main = do
  conn <- connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  res <- quickQuery conn "select * from tbl02" []
  putStrLn $ show res
  let r1 = head res
  print r1
  let [r1c1, r1c2] = r1
  print r1c2
  let vdt1 = fromSql r1c2 :: Day
  print vdt1

  



  
