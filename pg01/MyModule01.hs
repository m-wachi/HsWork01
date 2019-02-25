module MyModule01 where

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

{- 1週間1レコードの型 -}
data SalesNumWeek = SalesNumWeek {
  snwDlvDateMonN8 :: Int, -- 月曜日の日付値(YYYYMMDD)
  snwDlvDateMonNum :: Int,  -- 月曜日の日付値(Date.Time.Calendar)
  snwCustomerCode :: Int,
  snwStoreCd :: Int,
  snwMercCd :: Int,
  snwQtyMon :: Int,
  snwQtyTue :: Int,
  snwQtyWed :: Int,
  snwQtyThu :: Int,
  snwQtyFri :: Int,
  snwQtySat :: Int,
  snwQtySun :: Int
  } deriving (Show)


{-
  Convert Julian day to YYYYMMDD
-}
dateNum2N8 :: Int -> Int
dateNum2N8 dateNum
  = ((fromIntegral y) * 10000) + (m * 100) + d
  where
    (y, m, d) = Cal.toGregorian $ Cal.ModifiedJulianDay $ fromIntegral dateNum


{-
  タプルをSalesNumWeekに変換
-}
fromTpl2SNW :: (Int, Int, Int, Int, Int, Int, Int, Int) -> SalesNumWeek
fromTpl2SNW (ddMonN8, dd, cc, sc, mc, qt, dnMon, dow)
  | dow == 1 = SalesNumWeek ddMonN8 dnMon cc sc mc qt 0 0 0 0 0 0
  | dow == 2 = SalesNumWeek ddMonN8 dnMon cc sc mc qt 0 0 0 0 0 0
  | dow == 3 = SalesNumWeek ddMonN8 dnMon cc sc mc qt 0 0 0 0 0 0
  | dow == 4 = SalesNumWeek ddMonN8 dnMon cc sc mc 0 0 0 qt 0 0 0
  | dow == 5 = SalesNumWeek ddMonN8 dnMon cc sc mc 0 0 0 0 qt 0 0
  | dow == 6 = SalesNumWeek ddMonN8 dnMon cc sc mc 0 0 0 0 0 qt 0
  | dow == 7 = SalesNumWeek ddMonN8 dnMon cc sc mc 0 0 0 0 0 0 qt

{-
  レコードデータをタプルに変換
-}
fromSql2Tpl :: [HDBC.SqlValue] -> (Int, Int, Int, Int, Int, Int, Int, Int)
fromSql2Tpl [dd, cc, sc, mc, qt, dn, wn, dow]
    = (ddMonN8, dd2, cc2, sc2, mc2, qt2, dnMon, dow2)
  where
    ddMonN8 = dateNum2N8 dnMon
    dd2 = (truncate $ fromRational $ HDBC.fromSql dd) :: Int
    cc2 = (truncate $ fromRational $ HDBC.fromSql cc) :: Int
    sc2 = (truncate $ fromRational $ HDBC.fromSql sc) :: Int
    mc2 = (truncate $ fromRational $ HDBC.fromSql mc) :: Int
    qt2 = (truncate $ fromRational $ HDBC.fromSql qt) :: Int
    dn2 = truncate $ fromRational $ HDBC.fromSql dn :: Int
    dow2 = HDBC.fromSql dow :: Int
    dnMon = dn2 - dow2

fromSql2SNW :: [HDBC.SqlValue] -> SalesNumWeek
fromSql2SNW = fromTpl2SNW . fromSql2Tpl

{-
  月曜日付値、得意先コード、店舗コード、商品コード一致判定
-}
isSameGroup01 :: SalesNumWeek -> SalesNumWeek -> Bool
isSameGroup01 snw1 snw2
    = (snwDlvDateMonNum snw1) == (snwDlvDateMonNum snw2)
      && (snwCustomerCode snw1) == (snwCustomerCode snw2) 
      && (snwMercCd snw1) == (snwMercCd snw2)

{-
  月曜日付値、得意先コード、店舗コード、商品コードでグルーピング
-}
splitGroup01 :: [[SalesNumWeek]] -> [SalesNumWeek] -> [[SalesNumWeek]]
splitGroup01 acc snws
  = if null a1
    then acc
    else splitGroup01 (acc ++ [a1]) a2
  where
    snwHead = head snws
    (a1, a2) = span (isSameGroup01 snwHead) snws

mergeSNW :: SalesNumWeek -> SalesNumWeek -> SalesNumWeek
mergeSNW snw1 snw2
    = SalesNumWeek ddmN8 ddmNum cc sc mc qm qtu qw qth qf qsa qsu
  where
    ddmN8 = snwDlvDateMonN8 snw1
    ddmNum = snwDlvDateMonNum snw1
    cc = snwCustomerCode snw1
    sc = snwStoreCd snw1
    mc = snwMercCd snw1
    qm = (snwQtyMon snw1) + (snwQtyMon snw2)
    qtu = (snwQtyTue snw1) + (snwQtyTue snw2)
    qw = (snwQtyWed snw1) + (snwQtyWed snw2)
    qth = (snwQtyThu snw1) + (snwQtyThu snw2)
    qf = (snwQtyFri snw1) + (snwQtyFri snw2)
    qsa = (snwQtySat snw1) + (snwQtySat snw2)
    qsu = (snwQtySun snw1) + (snwQtySun snw2)
    

accumSNW :: [SalesNumWeek] -> SalesNumWeek
accumSNW snws = foldr mergeSNW (head snws) $ tail snws

insTSalesNumWeek :: PgHDBC.Connection -> SalesNumWeek -> IO Integer
insTSalesNumWeek conn salesNumWeek = do
  let sqlDlvDateNMon = HDBC.toSql $ snwDlvDateMonN8 salesNumWeek
  let sqlCustomerCd = HDBC.toSql $ snwCustomerCode salesNumWeek
  let sqlStoreCd = HDBC.toSql $ snwStoreCd salesNumWeek
  let sqlMercCd = HDBC.toSql $ snwMercCd salesNumWeek
  let sqlQtyMon = HDBC.toSql $ snwQtyMon salesNumWeek
  let sqlQtyTue = HDBC.toSql $ snwQtyTue salesNumWeek
  let sqlQtyWed = HDBC.toSql $ snwQtyWed salesNumWeek
  let sqlQtyThu = HDBC.toSql $ snwQtyThu salesNumWeek
  let sqlQtyFri = HDBC.toSql $ snwQtyFri salesNumWeek
  let sqlQtySat = HDBC.toSql $ snwQtySat salesNumWeek
  let sqlQtySun = HDBC.toSql $ snwQtySun salesNumWeek
  HDBC.run conn "insert into t_sales_num02 values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    [sqlDlvDateNMon, sqlCustomerCd, sqlStoreCd, sqlMercCd,
     sqlQtyMon, sqlQtyTue, sqlQtyWed, sqlQtyThu, sqlQtyFri, sqlQtySat, sqlQtySun]

