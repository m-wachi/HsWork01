insert into t_sales_num01
select delivery_date_n, customer_cd, store_cd, merc_cd, sum(quantity) 
from wk_sales_num01
group by delivery_date_n, customer_cd, store_cd, merc_cd;
