

create table t_sales_num02(
    dlv_date_n_mon numeric(8) not null, -- 月曜出荷日
    customer_cd numeric(6) not null, -- 得意先コード
    store_cd numeric(6) not null, -- 店舗コード
    merc_cd numeric(6) not null, -- 商品コード
    qty_mon numeric(6) not null, -- 数量
    qty_tue numeric(6) not null,
    qty_wed numeric(6) not null,
    qty_thu numeric(6) not null,
    qty_fri numeric(6) not null,
    qty_sat numeric(6) not null,
    qty_sun numeric(6) not null,
    primary key (dlv_date_n_mon, customer_cd, store_cd, merc_cd)
);
