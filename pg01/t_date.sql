drop table t_date;

create table t_date(
    date_n8 numeric(8),
    date_dt date,
    date_num numeric(6),
    week_num numeric(6),
    day_of_week int,
    primary key (date_n8)
);
