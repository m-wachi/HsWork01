select merc_cd, customer_cd, count(*) from t_sales_num01 where merc_cd=105617 and customer_cd=941 group by merc_cd, customer_cd
;
