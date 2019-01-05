

create table t_sales_num01(
    delivery_date_n numeric(8) not null, -- 出荷日
    customer_cd numeric(6) not null, -- 得意先コード
    store_cd numeric(6) not null, -- 店舗コード
    merc_cd numeric(6) not null, -- 商品コード
    quantity numeric(6) not null, -- 数量
    primary key (delivery_date_n, customer_cd, store_cd, merc_cd)
);
