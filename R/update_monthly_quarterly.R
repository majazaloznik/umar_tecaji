###############################################################################
## mesečni in četrtletni update tabel tečajev
###############################################################################

###############################################################################
## Preliminaries
###############################################################################
home <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\umar_tecaji\\"
setwd(home)
library(gmailr)
library(kableExtra)
options(gargle_oauth_email = TRUE)
gm_auth_configure(path ="data/credentials.json")
gm_auth(email = TRUE, cache = ".secret")

email_list <- c("maja.zaloznik@gmail.com",
               "maja.zaloznik@gov.si",
               "rotija.kmet-zupancic@gov.si")

source("R/helper_functions.R")
print(email_produktivnost_update_body)
###############################################################################
## preveri velikosti tabel
###############################################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, enc2utf8('set search_path to "tecaji"'))

initial_counts <- get_produktivnost_table_row_counts(con)|>
  dplyr::filter(tabela != "tečaji_dnevni")

###############################################################################
## poženi skripto za mesečne in četrtletne tečaje
###############################################################################
source("R\\ecb_monthly_quarterly.R", encoding = 'UTF-8')


###############################################################################
## preveri novo velikosti tabel
###############################################################################
new_counts <- get_produktivnost_table_row_counts(con) |>
  dplyr::filter(tabela != "tečaji_dnevni")

diff <- initial_counts  |>
  full_join(new_counts, by = "tabela", suffix = c("_prej", "_zdej")) |>
  filter(vrstice_prej != vrstice_zdej) |>
  mutate(sprememba = vrstice_zdej - vrstice_prej) |>
  select(tabela, vrstice_prej, vrstice_zdej, sprememba)


# prepare email body
body <- email_produktivnost_update_body(diff)

# email changes to list of recipients
email_produktivnost_changes(body, recipient = email_list)

###############################################################################
## Wrap up
###############################################################################
DBI::dbDisconnect(con)
cat("script completed")
