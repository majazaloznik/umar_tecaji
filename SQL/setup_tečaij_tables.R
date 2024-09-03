# Load necessary libraries
library(DBI)
library(RPostgres)
library(dplyr)

# Assuming `data_macro` is already created as per the given code

# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "192.168.38.21",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS tecaji")
DBI::dbExecute(con, "set search_path to tecaji")

#### tečaji mesečni ############################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS tečaji_mesečni;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "tečaji_mesečni" (
    "freq" VARCHAR,
    "currency" VARCHAR,
    "obstime" DATE,
    "obsvalue" DOUBLE PRECISION,
    "type" VARCHAR,
    "year" INTEGER,
    "month" INTEGER,
    "yoy" DOUBLE PRECISION,
    "long_avg_TOT" DOUBLE PRECISION,
    "long_avg_2004" DOUBLE PRECISION,
    "m3avg" DOUBLE PRECISION,
    "m3avg_ver2" DOUBLE PRECISION,
    "I2007" DOUBLE PRECISION,
    "indeks_LTA" DOUBLE PRECISION,
    "indeks_2007" DOUBLE PRECISION,
     UNIQUE (currency, obstime, type));'
# Execute the table creation command
dbExecute(con, create_table_sql)


#### tečaji četrtletni  ########################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS tečaji_četrtletni;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "tečaji_četrtletni" (
    "freq" VARCHAR,
    "currency" VARCHAR,
    "obstime" DATE,
    "obsvalue" DOUBLE PRECISION,
    "type" VARCHAR,
    "year" INTEGER,
    "q" INTEGER,
    "month" INTEGER,
    "yoy" DOUBLE PRECISION,
    "long_avg_TOT" DOUBLE PRECISION,
    "long_Q3_2004" DOUBLE PRECISION,
    "indeks_LTA" DOUBLE PRECISION,
    "Q4avg" DOUBLE PRECISION,
    "I2007" DOUBLE PRECISION,
    "indeks_2007" DOUBLE PRECISION,
     UNIQUE (currency, obstime, type));'
# Execute the table creation command
dbExecute(con, create_table_sql)



#### preračuni  ################################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS preračuni;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "preračuni" (
    "freq" VARCHAR,
    "currency" VARCHAR,
    "obstime" DATE,
    "year" INTEGER,
    "month" INTEGER,
    "relHICP_EA_18" DOUBLE PRECISION,
    "relHICP_EA_41" DOUBLE PRECISION,
    "relHICP_EA" DOUBLE PRECISION,
    "relHICP_EURO" DOUBLE PRECISION,
    "relHICP_ZEA_18" DOUBLE PRECISION,
    "relULC" DOUBLE PRECISION,
     UNIQUE (freq, currency, obstime));'
# Execute the table creation command
dbExecute(con, create_table_sql)

#### tečaji mesečni non EA ############################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS nonEA_tečaji_mesečni;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "nonEA_tečaji_mesečni" (
    "freq" VARCHAR,
    "currency" VARCHAR,
    "obstime" DATE,
    "obsvalue" DOUBLE PRECISION,
    "type" VARCHAR,
    "year" INTEGER,
    "month" INTEGER,
    "yoy" DOUBLE PRECISION,
    "long_avg_TOT" DOUBLE PRECISION,
    "long_avg_2004" DOUBLE PRECISION,
    "m3avg" DOUBLE PRECISION,
    "m3avg_ver2" DOUBLE PRECISION,
    "I2007" DOUBLE PRECISION,
    "indeks_LTA" DOUBLE PRECISION,
    "indeks_2007" DOUBLE PRECISION,
     UNIQUE (currency, obstime, type));'
# Execute the table creation command
dbExecute(con, create_table_sql)

#### tečaji dnevni non EA ############################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS tečaji_dnevni;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "tečaji_dnevni" (
    "freq" VARCHAR,
    "currency" VARCHAR,
    "currency_denom" VARCHAR,
    "exr_type" VARCHAR,
    "exr_suffix" VARCHAR,
    "obstime" DATE,
    "obsvalue" DOUBLE PRECISION,
    "type" VARCHAR,
     UNIQUE (currency, obstime));'
# Execute the table creation command
dbExecute(con, create_table_sql)
