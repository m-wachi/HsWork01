import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PgHDBC
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWD -- package time-1.6.0.1
import qualified System.Directory as Dir
import qualified Data.List as DL

import qualified MyModule01 as MyM1

csv_dir = "/home/m-wachi-ipad/work/csv"

-- genDayList :: Cal.Day -> Cal.Day -> [Cal.Day]
-- genDayList from to =
--   takeWhile (<= to) $ iterate succ from


getCsvFullPath :: String -> String
getCsvFullPath fileName = csv_dir ++ "/" ++ fileName


putSalesNumFromCsv :: String -> IO Integer
putSalesNumFromCsv csvPath = do
  fileContents <- readFile csvPath
  let lineData = map init $ tail $ lines fileContents -- discard header, remove ¥r
  -- let f1 = (map MyM1.remove2Quote) . MyM1.splitComma
  -- print $ f1 $ head lineData
  -- let lineData2 = map f1 lineData
  let salesNums = map MyM1.getSalesNumFromCsvRec lineData
  -- print salesNums
  conn <- PgHDBC.connectPostgreSQL "host='localhost' dbname='user01db' user='user01' password='user01'"
  -- -- insRecord conn sn1
  d <- mapM (MyM1.insTSalesNum conn) salesNums -- use mapM if function is Action(IO Monad)
  HDBC.commit conn
  HDBC.disconnect conn
  let cnt = sum d
  print $ csvPath ++ ": cnt=" ++ (show cnt)
  return cnt


main = do
  lstFile <- Dir.getDirectoryContents csv_dir
  let f1 = head lstFile
  print f1
  print (MyM1.isExtCsv f1)
  let f2 = head $ tail lstFile
  print f2
  print (MyM1.isExtCsv f2)
  let lstFile2 = DL.sort $ filter MyM1.isExtCsv lstFile
  let f3 = head lstFile2
  print f3
  let filePath = getCsvFullPath f3
  print filePath
  let lstFile3 = take 3 lstFile2
  print lstFile3
  --recCnt <- putSalesNumFromCsv filePath
  --print ("record count = " ++ (show $ sum recCnt))
  mapM putSalesNumFromCsv $ map getCsvFullPath lstFile2
  -- -- res <- HDBC.quickQuery conn "select * from t_sales_num01" []
  -- -- putStrLn $ show res


  
