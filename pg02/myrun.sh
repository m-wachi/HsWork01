#!/bin/bash
date
psql -f trun02.sql
./ins_sale_num02 20170101 20170131
date
