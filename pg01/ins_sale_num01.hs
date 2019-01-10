import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1

-- insRecord :: PgHDBC.Connection -> Cal.Day -> IO Integer
-- insRecord conn dayVal = do
--   let dn = Cal.toModifiedJulianDay dayVal
--   -- dow: 1:Mon, 2:Tue ... 7:Sun
--   let (_, _, dow) = CalWD.toWeekDate dayVal
--   -- print dow
--   let tmp = (dn - 4 - (fromIntegral dow)) :: Integer
--   let wn = tmp `div` (7::Integer) 
--   -- print wn
--   -- let downum = CalW.fromEnum dow
--   HDBC.run conn "insert into t_date values(?, ?, ?, ?)"
--     [HDBC.toSql dayVal, HDBC.toSql dn, HDBC.toSql wn, HDBC.toSql dow]


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
  -- conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"

  fileContents <- readFile "result-h2.csv"
  let lineData = lines fileContents
  print (head lineData)
  print "---"
  print (tail lineData)
  print "---"
  let l2Data = head $ tail lineData
  let (col1, _) = break (== ',') l2Data
  print col1
  print "---"
  let cols = splitComma l2Data
  print cols
  print "---"
  let col1a = remove2Quote col1
  print col1a
  let cols2 = map remove2Quote cols
  print cols2
  print "---"
  let f1 = (map remove2Quote) . splitComma
  let cols3 = f1 l2Data
  print cols3
  print "---"
  let lineData2 = map f1 lineData
  print lineData2
  {-
  let a = Cal.fromGregorian 2015 1 1
  print a
  let b = Cal.fromGregorian 2015 1 10
  print b
  let c = genDayList a b
  -- print c
  let f1 = insRecord conn
  d <- mapM (insRecord conn) c -- use mapM if function is Action(IO Monad)
  -}
  -- print d
  -- HDBC.commit conn
  -- res <- HDBC.quickQuery conn "select * from t_date" []
  -- putStrLn $ show res
  -- HDBC.disconnect conn


  
