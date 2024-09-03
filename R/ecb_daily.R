################################################################################
####
####	Efektivni tečaji ECB - dnevni
####
####  revizija MZ avgust 2024, zadnja revizija Matevž/Katarina februar 2023
################################################################################
#### Knjižnice
library(ecb)
library(dplyr)
#### setup
source("R/helper_functions.R")

######################################## TECAJ  ################################
EURO_ER_D <- get_data(paste0("EXR.D.",target_nonEA,".EUR.SP00.A"),
                      filter = list(startPeriod = "2015-01-01")) |>
  mutate(type ="EURO_ER_D")


################################################################################
#            zapis na postgres bazo                                            #
################################################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to tecaji")

DBI::dbExecute(con, "TRUNCATE TABLE \"tečaji_dnevni\"")

# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, enc2utf8("tečaji_dnevni"), EURO_ER_D, append = TRUE, row.names = FALSE)

