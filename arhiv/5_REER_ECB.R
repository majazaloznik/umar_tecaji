ptm <- proc.time()
###################################################################################################################
####  Matevž (Katarina osvežitev ob preimenovanju serij)
####
####	Efektivni tečaji ECB
####  Zadnja sprememba: februar 23
####
####  Precej stara skripta, ki ok deluje, ampak bi se jo dalo precej optimizirat
###################################################################################################################

### Knjižnice

library(ecb)
library(tidyr)
library(dplyr)
library(RODBC)
library(httr)        #delo s http (download podatkov)
library(data.table)
library(zoo)

#brisanje vseh dosedanjih spremenljivk
rm(list=ls())
ls()

# #hitrost ra?unanje
# ptm <- proc.time()

#### 	priprava okolja za delo
setwd("O:/Users/KIvas/R")

# set proxy za download xml
set_config(use_proxy(url = "http://proxy.gov.si", port = 80))

##################################################
#FUNKCIJA ZA YOY, V %
YearOverYear<-function (x,periodsPerYear){
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
  }
  else{
    indexes<-1:(NROW(x)-periodsPerYear)
    return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])/x[indexes]*100))
  }

}
#######################################################################################################

target_EA <- c("ATS", "BEF", "CYP", "DEM", "EEK", "ESP", "FIM", "FRF", "GRD", "HRK",
            "IEP", "ITL", "LTL", "LUF", "LVL", "MTL", "NLG", "PTE", "SIT", "SKK")

target_nonEA <- c("CHF", "CNY", "CZK", "GBP", "HUF", "JPY", "KRW", "PLN", "RUB", "TRY", "USD", "DKK", "RON", "SEK", "AUD", "BGN", "CAD", "HKD", "NOK", "SGD")
######################################## TECAJ
####EURO do VALUT D
#samo za uporabo v napovedi
key <- "EXR.D..EUR.SP00.A"
df <- get_data(key, filter = NULL)

EURO_ER_D <-
  df%>%
  filter(obstime>"2015-01-01" & currency %in% target_nonEA)


####EURO do VALUT M
key <- "EXR.M..EUR.SP00.A"
df <- get_data(key, filter = NULL)   #library ecb
EURO_ER_M <- df%>%
   filter(currency %in% target_nonEA)


# EER EA+18 : EA +  Australia, Canada, Denmark, Hong Kong, Japan, Korea, Norway, Singapore, Sweden,
# Switzerland, the United Kingdom, the United States, Bulgaria, China, Czech Republic, Hungary, Poland and Romania

# EER EA+41 : EA + 18 + Algeria, Argentina, Brazil, Chile, Colombia, Iceland, India, Indonesia, Israel, Malaysia,
# Mexico, Morocco, New Zealand, Peru, the Philippines, Russia, Saudi Arabia, South Africa, Taiwan, Thailand, Turkey,
# Ukraine and the United Arab Emirates.

######################################## NEER
####NEER EA+18 M
key <- "EXR.M.H02..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_18_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EA+18 Q
key <- "EXR.Q.H02..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_18_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EA+41 M
key <- "EXR.M.H03..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_41_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER ZEA 41 M
key <- "EXR.M.E03..NN00.A"
df <- get_data(key, filter = NULL)
NEER_ZEA_41_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EA+41 Q
key <- "EXR.Q.H03..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_41_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EA M
key <- "EXR.M.H00..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EA Q
key <- "EXR.Q.H00..NN00.A"
df <- get_data(key, filter = NULL)
NEER_EA_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER EURO M
key <- "EXR.M.E02.EUR.EN00.A"
NEER_EURO_18_M <- get_data(key, filter = NULL)

####NEER EURO Q
key <- "EXR.Q.E02.EUR.EN00.A"
NEER_EURO_18_Q <- get_data(key, filter = NULL)

####NEER ZEA 18 M
key <- "EXR.M.E02..NN00.A"
df <- get_data(key, filter = NULL)
NEER_ZEA_18_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####NEER ZEA 18 Q
key <- "EXR.Q.E02..NN00.A"
df <- get_data(key, filter = NULL)
NEER_ZEA_18_Q <-
  df%>%
  filter(currency_denom %in% target_EA)


######################################## REER
####REER EA+18 M
key <- "EXR.M.H02..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_18_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EA+18 Q
key <- "EXR.Q.H02..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_18_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EA+41 M
key <- "EXR.M.H03..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_41_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EA+41 Q
key <- "EXR.Q.H03..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_41_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EA M
key <- "EXR.M.H00..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EA Q
key <- "EXR.Q.H00..NRC0.A"
df <- get_data(key, filter = NULL)
REER_EA_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER EURO_M
key <- "EXR.M.E02.EUR.ERC0.A"
REER_EURO_18_M <- get_data(key, filter = NULL)

####REER EURO_Q
key <- "EXR.Q.E02.EUR.ERC0.A"
REER_EURO_18_Q <- get_data(key, filter = NULL)

####REER ZEA 18 M
key <- "EXR.M.E02..NRC0.A"
df <- get_data(key, filter = NULL)
REER_ZEA_18_M <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER ZEA 18 Q
key <- "EXR.Q.E02..NRC0.A"
df <- get_data(key, filter = NULL)
REER_ZEA_18_Q <-
  df%>%
  filter(currency_denom %in% target_EA)

####REER ULC Q
key <- "EXR.Q.H02..NRU1.A"
REER_ULC_Q <- get_data(key, filter = NULL)

key <- "EXR.Q.H00..NRU1.A"
REER_ULC_EA_Q <- get_data(key, filter = NULL)

key <- "EXR.Q.E02..NRU1.A"
REER_ULC_ZEA_Q <- get_data(key, filter = NULL)

####REER BDP Q
key <- "EXR.Q.H02..NRD0.A"
REER_BDP_Q <- get_data(key, filter = NULL)

####REER PPI Q
key <- "EXR.Q.H02..NRP0.A"
REER_PPI_Q <- get_data(key, filter = NULL)

key <- "EXR.Q.H00..NRP0.A"
REER_PPI_EA_Q <- get_data(key, filter = NULL)

key <- "EXR.Q.E02..NRP0.A"
REER_PPI_ZEA_Q <- get_data(key, filter = NULL)

### Lahko dodam ?e mese?no REER_PPI

rm(df)

gc
gc (verbose = T)

#######################################################################
############### preracuni
######################################################################
# dodajanje spremenljivke z imenom datoteke v dataset
EURO_ER_M <- EURO_ER_M %>% mutate(type ="EURO_ER_M")
EURO_ER_D <- EURO_ER_D %>% mutate(type ="EURO_ER_D")
NEER_EA_18_M <- NEER_EA_18_M %>% mutate(type ="NEER_EA_18_M")
NEER_EA_18_Q <- NEER_EA_18_Q %>% mutate(type ="NEER_EA_18_Q")
NEER_EA_41_M <- NEER_EA_41_M %>% mutate(type ="NEER_EA_41_M")
NEER_EA_41_Q <- NEER_EA_41_Q %>% mutate(type ="NEER_EA_41_Q")
NEER_EA_M <- NEER_EA_M %>% mutate(type ="NEER_EA_M")
NEER_EA_Q <- NEER_EA_Q %>% mutate(type ="NEER_EA_Q")
NEER_EURO_18_M <- NEER_EURO_18_M %>% mutate(type ="NEER_EURO_18_M")
NEER_EURO_18_Q <- NEER_EURO_18_Q %>% mutate(type ="NEER_EURO_18_Q")
NEER_ZEA_18_M <- NEER_ZEA_18_M %>% mutate(type ="NEER_ZEA_18_M")
NEER_ZEA_18_Q <- NEER_ZEA_18_Q %>% mutate(type ="NEER_ZEA_18_Q")
REER_EA_18_M <- REER_EA_18_M %>% mutate(type ="REER_EA_18_M")
REER_EA_18_Q <- REER_EA_18_Q %>% mutate(type ="REER_EA_18_Q")
REER_EA_41_M <- REER_EA_41_M %>% mutate(type ="REER_EA_41_M")
REER_EA_41_Q <- REER_EA_41_Q %>% mutate(type ="REER_EA_41_Q")
REER_EA_M <- REER_EA_M %>% mutate(type ="REER_EA_M")
REER_EA_Q <- REER_EA_Q %>% mutate(type ="REER_EA_Q")
REER_EURO_18_M <- REER_EURO_18_M %>% mutate(type ="REER_EURO_18_M")
REER_EURO_18_Q <- REER_EURO_18_Q %>% mutate(type ="REER_EURO_18_Q")
REER_ULC_Q <- REER_ULC_Q %>% mutate(type ="REER_ULC_Q")
REER_ZEA_18_M <- REER_ZEA_18_M %>% mutate(type ="REER_ZEA_18_M")
REER_ZEA_18_Q <- REER_ZEA_18_Q %>% mutate(type ="REER_ZEA_18_Q")
REER_PPI_Q <- REER_PPI_Q %>% mutate(type ="REER_PPI_Q")
REER_BDP_Q <- REER_BDP_Q %>% mutate(type ="REER_BDP_Q")
REER_PPI_EA_Q <- REER_PPI_EA_Q %>% mutate(type ="REER_PPI_EA_Q")
REER_PPI_ZEA_Q <- REER_PPI_ZEA_Q %>% mutate(type ="REER_PPI_ZEA_Q")
REER_ULC_EA_Q <- REER_ULC_EA_Q %>% mutate(type ="REER_ULC_EA_Q")
REER_ULC_ZEA_Q <- REER_ULC_ZEA_Q %>% mutate(type ="REER_ULC_ZEA_Q")

#zdruzevanje fajlov - loceno po M in Q
df_M <- rbindlist(list(NEER_EA_18_M, NEER_EA_41_M, NEER_EA_M, NEER_EURO_18_M,
                       NEER_ZEA_18_M, REER_EA_18_M, REER_EA_41_M, REER_EA_M,
                       REER_EURO_18_M, REER_ZEA_18_M))

names(df_M)[2] <- "currency_denom"
names(df_M)[3] <- "currency"
df_M <- rbind(df_M,EURO_ER_M )

df_Q <- rbindlist(list(NEER_EA_18_Q, NEER_EA_41_Q, NEER_EA_Q, NEER_EURO_18_Q, NEER_ZEA_18_Q,
                       REER_EA_18_Q, REER_EA_41_Q, REER_EA_Q, REER_EURO_18_Q, REER_ZEA_18_Q,
                       REER_ULC_Q, REER_BDP_Q, REER_PPI_Q, REER_PPI_EA_Q, REER_PPI_ZEA_Q, REER_ULC_EA_Q, REER_ULC_ZEA_Q))
names(df_Q)[2] <- "currency_denom"
names(df_Q)[3] <- "currency"


############# obdelava mesecnih podatkov
df <-   df_M

#definicija spremenljivk
df$obstime <- as.Date(paste(df$obstime, "01", sep ="-"))
df$obsvalue <- as.numeric(as.character(df$obsvalue))

#preoblikovanje stolpcev
df$year <- format(as.Date(df$obstime, format="%Y-%m-%d"),"%Y")
df$month <- format(as.Date(df$obstime, format="%Y-%m-%d"),"%m")

#preracun yoy rasti in dolgoletnega  povprecja od jul2004 naprej
df_M <-
  df %>%
  dplyr:: select(-currency_denom, -exr_suffix, -exr_type)%>%
  dplyr:: group_by(type, currency) %>%
  dplyr:: mutate (yoy = YearOverYear(obsvalue,12)) %>%
  dplyr:: mutate(long_avg_TOT = mean(obsvalue))%>%
  dplyr:: mutate(long_avg_2004 = mean(obsvalue[obstime >= as.Date("2004-07-01")]))%>%
  dplyr:: mutate(m3avg = rollmean(obsvalue,3, fill=NA))%>%
  dplyr:: mutate(m3avg_ver2 = rollmean(obsvalue,3, fill=NA, align = "right"))%>%
  dplyr:: mutate(I2007 = mean(obsvalue[obstime >= as.Date("2007-01-01")& obstime <= as.Date("2007-12-01")])) %>%
  dplyr:: mutate(indeks_LTA = obsvalue / long_avg_2004 * 100)%>%
  dplyr:: mutate(indeks_2007 = obsvalue / I2007 * 100)%>%
  ungroup()

df_tecaj<-
  df_M%>%
  filter(type == "EURO_ER_M")

df_M <-
  df_M%>%
  filter(type != "EURO_ER_M")


############# obdelava cetrtletnih podatkov
df <-   df_Q

#preoblikovanje zapisa cetrtletij v mesece
df$year <- substr(df$obstime, 1, 4)
df$q <-substr(df$obstime, 7, 7)
df$month <- ifelse(df$q =="1","01",
                   ifelse(df$q =="2","04",
                          ifelse(df$q =="3","07",
                                 "10")))

#definicija spremenljivk
df$obstime <- as.Date(paste(df$year,df$month, "01", sep ="-"))
df$obsvalue <- as.numeric(as.character(df$obsvalue))

#preracun yoy rasti in dolgoletnega povprecja od Q3-2004 naprej
df_Q <-
  df %>%
  dplyr:: select(-currency_denom, -exr_suffix, -exr_type)%>%
  dplyr:: group_by(type, currency) %>%
  dplyr:: mutate (yoy = YearOverYear(obsvalue,4)) %>%
  dplyr:: mutate(long_avg_TOT = mean(obsvalue))%>%
  dplyr:: mutate(long_Q3_2004 = mean(obsvalue[obstime >= as.Date("2004-07-01")]))%>%
  dplyr:: mutate(indeks_LTA = obsvalue / long_Q3_2004 * 100)%>%
  dplyr:: mutate(Q4avg = rollmean(obsvalue,4, na.pad=TRUE, align = "right"))%>%
  dplyr:: mutate(I2007 = mean(obsvalue[obstime >= as.Date("2007-01-01")& obstime <= as.Date("2007-12-01")])) %>%
  dplyr:: mutate(indeks_2007 = obsvalue / I2007 * 100)%>%
  ungroup()

#izracuni  relativni ULC in relativni HICP
preracuni_M <-
  df_M%>%
  dplyr:: select(freq, currency, obstime, obsvalue, type, year, month)%>%
  dplyr:: group_by(currency)%>%
  spread(key=type, obsvalue)%>%
  dplyr:: mutate(relHICP_EA_18 = REER_EA_18_M / NEER_EA_18_M)%>%
  dplyr:: mutate(relHICP_EA_41 = REER_EA_41_M / NEER_EA_41_M)%>%
  dplyr:: mutate(relHICP_EA = REER_EA_M / NEER_EA_M)%>%
  dplyr:: mutate(relHICP_EURO = REER_EURO_18_M / NEER_EURO_18_M)%>%
  dplyr:: mutate(relHICP_ZEA_18 = REER_ZEA_18_M / NEER_ZEA_18_M)%>%
  select(freq, currency, obstime, year, month,
         relHICP_EA_18, relHICP_EA_41, relHICP_EA,
         relHICP_EURO, relHICP_ZEA_18)

preracuni_M$relULC <- NA

preracuni_Q <-
  df_Q%>%
  select(freq, currency, obstime, obsvalue, type, year, month)%>%
  dplyr:: group_by(currency)%>%
  spread(key=type, obsvalue)%>%
  dplyr:: mutate(relULC = REER_ULC_Q / NEER_EA_18_Q)%>%
  dplyr:: mutate(relHICP_EA_18 = REER_EA_18_Q / NEER_EA_18_Q)%>%
  dplyr:: mutate(relHICP_EA_41 = REER_EA_41_Q / NEER_EA_41_Q)%>%
  dplyr:: mutate(relHICP_EA = REER_EA_Q / NEER_EA_Q)%>%
  dplyr:: mutate(relHICP_EURO = REER_EURO_18_Q / NEER_EURO_18_Q)%>%
  dplyr:: mutate(relHICP_ZEA_18 = REER_ZEA_18_Q / NEER_ZEA_18_Q)%>%
  select(freq, currency, obstime, year, month, relULC,
         relHICP_EA_18, relHICP_EA_41, relHICP_EA,
         relHICP_EURO, relHICP_ZEA_18)


preracuni <- data.frame(rbind(preracuni_M, preracuni_Q))


df_tecaj$obstime <- as.character(df_tecaj$obstime)
df_M$obstime <- as.character(df_M$obstime)
df_Q$obstime <- as.character(df_Q$obstime)
preracuni$obstime <- as.character(preracuni$obstime)

df_tecaj <- as.data.frame(df_tecaj)

# access
require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/Efektivni_tecaji.accdb"))
try(sqlDrop(conn, "tecajRAW", errors = TRUE), silent = TRUE)
sqlSave(conn,df_tecaj, tablename= "tecajRAW")
try(sqlDrop(conn, "mesecniRAW", errors = FALSE), silent = TRUE)
sqlSave(conn,df_M, tablename= "mesecniRAW")
try(sqlDrop(conn, "cetrtletniRAW", errors = FALSE), silent = TRUE)
sqlSave(conn,df_Q, tablename= "cetrtletniRAW")
try(sqlDrop(conn, "preracuniRAW", errors = FALSE), silent = TRUE)
sqlSave(conn,preracuni, tablename= "preracuniRAW")
try(sqlDrop(conn, "tecaj_dnevni", errors = FALSE), silent = TRUE)
sqlSave(conn,EURO_ER_D, tablename= "tecaj_dnevni")
close(conn)

proc.time() - ptm




