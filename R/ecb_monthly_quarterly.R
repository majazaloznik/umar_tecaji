################################################################################
####
####	Efektivni tečaji ECB
####
####  revizija MZ avgust 2024, zadnja revizija Matevž/Katarina februar 2023
################################################################################
#### Knjižnice
library(ecb)
library(tidyr)
library(dplyr)
library(purrr)
library(zoo)
#### setup
source("R/helper_functions.R")

####EURO do VALUT M
EURO_ER_M <- get_data(paste0("EXR.M.",target_nonEA,".EUR.SP00.A"))

################################################################################
#### download NEER
################################################################################
NEER_EA_18_M <- get_data(paste0("EXR.M.H02.",target_EA,".NN00.A"))
NEER_EA_41_M <- get_data(paste0("EXR.M.H03.",target_EA,".NN00.A"))
NEER_ZEA_41_M <- get_data(paste0("EXR.M.E03.",target_EA,".NN00.A"))
NEER_EA_M <- get_data(paste0("EXR.M.H00.",target_EA,".NN00.A"))
NEER_EURO_18_M <- get_data("EXR.M.E02.EUR.EN00.A")
NEER_ZEA_18_M <- get_data(paste0("EXR.M.E02.",target_EA,".NN00.A"))

NEER_EA_18_Q <- get_data(paste0("EXR.Q.H02.",target_EA,".NN00.A"))
NEER_EA_41_Q <- get_data(paste0("EXR.Q.H03.",target_EA,".NN00.A"))
NEER_EA_Q <- get_data(paste0("EXR.Q.H00.",target_EA,".NN00.A"))
NEER_EURO_18_Q <- get_data("EXR.Q.E02.EUR.EN00.A")
NEER_ZEA_18_Q <- get_data(paste0("EXR.Q.E02.",target_EA,".NN00.A"))

################################################################################
#### download REER
################################################################################
REER_EA_18_M <- get_data(paste0("EXR.M.H02.",target_EA,".NRC0.A"))
REER_EA_41_M <- get_data(paste0("EXR.M.H03.",target_EA,".NRC0.A"))
REER_EA_M <- get_data(paste0("EXR.M.H00.",target_EA,".NRC0.A"))
REER_EURO_18_M <- get_data("EXR.M.E02.EUR.ERC0.A")
REER_ZEA_18_M <- get_data(paste0("EXR.M.E02.",target_EA,".NRC0.A"))

REER_EA_18_Q <- get_data(paste0("EXR.Q.H02.",target_EA,".NRC0.A"))
REER_EA_41_Q <- get_data(paste0("EXR.Q.H03.",target_EA,".NRC0.A"))
REER_EA_Q <- get_data(paste0("EXR.Q.H00.",target_EA,".NRC0.A"))
REER_EURO_18_Q <- get_data("EXR.Q.E02.EUR.ERC0.A")
REER_ZEA_18_Q <- get_data(paste0("EXR.Q.E02.",target_EA,".NRC0.A"))
REER_ULC_Q <- get_data("EXR.Q.H02..NRU1.A")
REER_ULC_EA_Q <- get_data("EXR.Q.H00..NRU1.A")
REER_ULC_ZEA_Q <- get_data("EXR.Q.E02..NRU1.A")
REER_BDP_Q <- get_data("EXR.Q.H02..NRD0.A")
REER_PPI_Q <- get_data("EXR.Q.H02..NRP0.A")
REER_PPI_EA_Q <- get_data("EXR.Q.H00..NRP0.A")
REER_PPI_ZEA_Q <- get_data("EXR.Q.E02..NRP0.A")

################################################################################
#### združevanje tabel
################################################################################
df_M <- bind_exchange_rate_tables(NEER_EA_18_M, NEER_EA_41_M, NEER_EA_M, NEER_EURO_18_M,
                          NEER_ZEA_18_M, REER_EA_18_M, REER_EA_41_M, REER_EA_M,
                          REER_EURO_18_M, REER_ZEA_18_M) |>
  bind_rows(EURO_ER_M |> mutate(type = "EURO_ER_M")) |>
  relocate(type, .after = obsvalue)

df_Q <- bind_exchange_rate_tables(NEER_EA_18_Q, NEER_EA_41_Q, NEER_EA_Q, NEER_EURO_18_Q,
                                  NEER_ZEA_18_Q, REER_EA_18_Q, REER_EA_41_Q, REER_EA_Q,
                                  REER_EURO_18_Q, REER_ZEA_18_Q, REER_ULC_Q, REER_BDP_Q,
                                  REER_PPI_Q, REER_PPI_EA_Q, REER_PPI_ZEA_Q, REER_ULC_EA_Q,
                                  REER_ULC_ZEA_Q) |>
  relocate(type, .after = obsvalue)

################################################################################
#### obdelava mesecnih podatkov
################################################################################
df_M <- df_M |>
  mutate(obstime = as.Date(paste(obstime, "01", sep ="-")),
         obsvalue = as.numeric(obsvalue),
         year = lubridate::year(obstime),
         month = lubridate::month(obstime)) |>
  select(-currency_denom, -exr_suffix, -exr_type) |>
  dplyr:: group_by(type, currency) |>
  mutate (yoy = year_over_year(obsvalue,12),
          long_avg_TOT = mean(obsvalue),
          long_avg_2004 = mean(obsvalue[obstime >= as.Date("2004-07-01")]),
          m3avg = rollmean(obsvalue,3, fill=NA),
          m3avg_ver2 = rollmean(obsvalue,3, fill=NA, align = "right"),
          I2007 = mean(obsvalue[obstime >= as.Date("2007-01-01")& obstime <= as.Date("2007-12-01")]),
          indeks_LTA = obsvalue / long_avg_2004 * 100,
          indeks_2007 = obsvalue / I2007 * 100) |>
  ungroup()

df_tecaj <-  df_M  |>
  filter(type == "EURO_ER_M")

df_M <-  df_M  |>
  filter(type != "EURO_ER_M")

#izracuni  relativni ULC in relativni HICP
preracuni_M <- df_M |>
  select(freq, currency, obstime, obsvalue, type, year, month) |>
  group_by(currency) |>
  spread(key=type, obsvalue) |>
  mutate(relHICP_EA_18 = REER_EA_18_M / NEER_EA_18_M,
         relHICP_EA_41 = REER_EA_41_M / NEER_EA_41_M,
         relHICP_EA = REER_EA_M / NEER_EA_M,
         relHICP_EURO = REER_EURO_18_M / NEER_EURO_18_M,
         relHICP_ZEA_18 = REER_ZEA_18_M / NEER_ZEA_18_M, .keep= "unused")

################################################################################
#### obdelava cetrtletnih podatkov
################################################################################
df_Q <- df_Q |>
  mutate(obstime = as.Date(as.yearqtr(obstime, format = "%Y-Q%q")),
         obsvalue = as.numeric(obsvalue),
         year = lubridate::year(obstime),
         q = lubridate::quarter(obstime),
         month = lubridate::month(obstime)) |>
  select(-currency_denom, -exr_suffix, -exr_type) |>
  group_by(type, currency) |>
  mutate(yoy = year_over_year(obsvalue,4),
         long_avg_TOT = mean(obsvalue),
         long_Q3_2004 = mean(obsvalue[obstime >= as.Date("2004-07-01")]),
         indeks_LTA = obsvalue / long_Q3_2004 * 100,
         Q4avg = rollmean(obsvalue,4, na.pad=TRUE, align = "right"),
         I2007 = mean(obsvalue[obstime >= as.Date("2007-01-01") & obstime <= as.Date("2007-12-01")]),
         indeks_2007 = obsvalue / I2007 * 100) |>
ungroup()

#izracuni  relativni ULC in relativni HICP
preracuni_Q <- df_Q |>
  select(freq, currency, obstime, obsvalue, type, year, month) |>
  group_by(currency) |>
  spread(key=type, obsvalue) |>
  mutate(relULC = REER_ULC_Q / NEER_EA_18_Q,
         relHICP_EA_18 = REER_EA_18_Q / NEER_EA_18_Q,
         relHICP_EA_41 = REER_EA_41_Q / NEER_EA_41_Q,
         relHICP_EA = REER_EA_Q / NEER_EA_Q,
         relHICP_EURO = REER_EURO_18_Q / NEER_EURO_18_Q,
         relHICP_ZEA_18 = REER_ZEA_18_Q / NEER_ZEA_18_Q, .keep = "unused") |>
  select(-REER_BDP_Q, -REER_PPI_EA_Q, -REER_PPI_Q, -REER_PPI_ZEA_Q,
         -REER_ULC_EA_Q, -REER_ULC_ZEA_Q)

preracuni <- bind_rows(preracuni_M, preracuni_Q)

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

DBI::dbExecute(con, "TRUNCATE TABLE \"tečaji_mesečni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, enc2utf8("tečaji_mesečni"), df_M, append = TRUE, row.names = FALSE)

DBI::dbExecute(con, "TRUNCATE TABLE \"tečaji_četrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, enc2utf8("tečaji_četrtletni"), df_Q, append = TRUE, row.names = FALSE)

DBI::dbExecute(con, "TRUNCATE TABLE \"preračuni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, enc2utf8("preračuni"), preracuni, append = TRUE, row.names = FALSE)

DBI::dbExecute(con, "TRUNCATE TABLE \"nonEA_tečaji_mesečni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, enc2utf8("nonEA_tečaji_mesečni"), df_tecaj, append = TRUE, row.names = FALSE)



