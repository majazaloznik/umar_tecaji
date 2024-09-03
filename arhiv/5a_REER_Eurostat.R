#####################################################################################################
#                                 REER, NEER Eurostat                                               #
#                           Zadnja sprememba: November 2020                                         #
#####################################################################################################


##### Knjižnice #####

library(eurostat)
library(tidyr)
library(dplyr)
library(zoo)


##### 	Priprava delovnega okolja #####

rm(list=ls())
ls()

gc()
gc (verbose = T)

setwd("M:/Konkurencnost/R")


# IC37 = EU27 + 10 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland, United Kingdom and Turkey)
# Broad group (42) = IC37 + 5 other industrial countries (Russia, China, Brazil, South Korea and Hong Kong).

#################################################################################################
#                               M PODATKI                                                       #
#################################################################################################



##### Download Podatkov #####
m_EER_ulc_bulk <- get_eurostat("ert_eff_ic_m") 
m_EER_ulc_bulk <- m_EER_ulc_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki

# Filtriranje
m_EER <-  droplevels(subset(m_EER_ulc_bulk,exch_rt %in% c("NEER_EA19","NEER_EU27_2020","NEER_IC37", "NEER_IC42", "REER_EA19_CPI",  "REER_EU27_2020_CPI",
                                                           "REER_IC37_CPI", "REER_IC42_CPI") & time >= "2004-01-01"))

m_EER$unit<-NULL


##### Preračuni #####

m_EER <- 
  m_EER %>% 
  arrange(time)%>%
  group_by(geo, exch_rt)%>%
  dplyr::mutate(YOY = ((values /lag(values,12))*100-100))%>%
  dplyr::mutate(Indeks2007=values/(mean(values[time >= "2007-01-01"& time <= "2007-12-01"]))*100)%>% 
  dplyr::mutate(Indeks2007_3mds=rollmean(Indeks2007,3, fill=NA, align = "right"))%>%
  dplyr::mutate(LTA = mean(values[time >= as.Date("2004-07-01")]))%>%
  dplyr::mutate(Indeks_LTA = values / LTA * 100)%>%
  select(-LTA)%>%
  ungroup()





#################################################################################################
#                               Q PODATKI                                                       #
#################################################################################################



##### Download Podatkov #####
q_EER_ulc_bulk <- get_eurostat("ert_eff_ic_q") 
q_EER_ulc_bulk <- q_EER_ulc_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki


# Filtriranje
q_EER <-  droplevels(subset(q_EER_ulc_bulk,exch_rt %in% c("NEER_EA19","NEER_EU27_2020","NEER_IC37", "NEER_IC42", "REER_EA19_CPI", "REER_EA19_ULCT", "REER_EU27_2020_CPI",
                                               "REER_EU27_2020_ULCT", "REER_IC37_CPI", "REER_IC37_ULCT","REER_IC42_CPI") & time >= "2000-01-01"))
  
q_EER$unit<-NULL
q

##### Preračuni #####

q_EER <- 
  q_EER %>% 
  arrange(time)%>%
  group_by(geo, exch_rt)%>%
  dplyr::mutate(YOY = ((values /lag(values,4))*100-100))%>%
  dplyr::mutate(Indeks2007=values/(mean(values[time >= "2007-01-01"& time <= "2007-10-01"]))*100)%>% 
  dplyr:: mutate(Indeks2007_4cds=rollmean(Indeks2007,4, fill=NA, align = "right"))%>%
  dplyr:: mutate(LTA = mean(values[time >= as.Date("2004-07-01")]))%>%
  dplyr:: mutate(Indeks_LTA = values / LTA * 100)%>%
  select(-LTA)%>%
  ungroup()



##### Urejanje  #####


#dodati leto in mesec
q_EER$year <-substr(q_EER$time, 1, 4)
q_EER$quarter <-substr(q_EER$time, 6, 7)

#sprememba stevilk cetrtletij v drugo poimenovanje
q_EER$quarter [q_EER$quarter =="01" ] <- "Q1"
q_EER$quarter [q_EER$quarter =="04" ] <- "Q2"
q_EER$quarter [q_EER$quarter =="07" ] <- "Q3"
q_EER$quarter [q_EER$quarter =="10" ] <- "Q4"


#################################################################################################
#                               A PODATKI                                                       #
#################################################################################################

##### Download Podatkov #####
a_EER_ulc_bulk <- get_eurostat("ert_eff_ic_a") 
a_EER_ulc_bulk <- a_EER_ulc_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki


# Filtriranje
a_EER <-  droplevels(subset(a_EER_ulc_bulk,exch_rt %in% c("NEER_EA19","NEER_EU27_2020","NEER_IC37", "NEER_IC42", "REER_EA19_CPI", "REER_EA19_ULCT", "REER_EU27_2020_CPI",
                                                          "REER_EU27_2020_ULCT", "REER_IC37_CPI", "REER_IC37_ULCT","REER_IC42_CPI") & time >= "1995-01-01"))

a_EER$unit<-NULL


##### Preračuni #####

a_EER <- 
  a_EER %>% 
  arrange(time)%>%
  group_by(geo, exch_rt)%>%
  dplyr::mutate(YOY = ((values /lag(values,1))*100-100))%>%
  dplyr::mutate(Indeks2007=values/(values[time == "2007-01-01"])*100)%>% 
  dplyr:: mutate(LTA = mean(values[time == as.Date("2004-01-01")]))%>%
  dplyr:: mutate(Indeks_LTA = values / LTA * 100)%>%
  select(-LTA)%>%
  ungroup()



##### Urejanje  #####

a_EER$year <-substr(a_EER$time, 1, 4)

rm(a_EER_ulc_bulk,m_EER_ulc_bulk,q_EER_ulc_bulk)

require(writexl)
write_xlsx(list("a_EER" = a_EER, "m_EER" = m_EER, "q_EER" = q_EER), "tecaji_estat.xlsx")

###############################################################################################################
#                                     SLIKE                                                                   #
###############################################################################################################


####### splošne nastavitve za grafe
library(extrafont)
library(gridExtra)
library(ggplot2)
loadfonts(device="win",quiet = T) #za Arial pisavo
#font_import()
fonts()

# rdeca	#9E001A ; svetlo roza   #D99694; svetlo siva	#949494;  temno siva	#535353;
# nafta #3F8B94; svetlo zelena  #9FCDAB; bela	#FFFFFF; crna	#000000
colourUMAR <- c("#000000", "#9E001A", "#949494", "#DBDBDB", "#44546A", "#A6A6A6", "#54A4A3", "#BBD2B0", "#6F3B66")   #barvna paleta

theme_UMAR <- function(){ 
  
  theme_bw () + theme(    #replace elements you want to change
    text=element_text(family="Arial", size=10),
    plot.title = element_text(face = "bold",hjust = 0,vjust = 2, size=10, family="Arial"), 
    plot.caption = element_text(hjust = 0, size=9, family="Arial"),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, color="black",size=10, family="Arial"),
    axis.text.y = element_text(hjust=1, vjust=0.5, color="black",size=10,family="Arial"),
    axis.title.x  = element_blank(),
    #axis.ticks = element_blank(),
    legend.position = "top",
    legend.justification="left",
    legend.title=element_blank(), # Legend title
    legend.text=element_text(size=10),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(0.3,"cm"),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey", fill=NA, size=0.5))
  
}

# NEER, REER q LTA


p1<- q_EER %>% filter((time >= "2008-01-01")&(exch_rt%in% c("NEER_IC37", "REER_IC37_CPI","REER_IC37_ULCT"))&geo=="SI")%>%select("Indeks_LTA", "time","exch_rt")%>%
 ggplot(aes(x=factor(time), y=Indeks_LTA, color=exch_rt))+
  geom_line(size=1, aes(group=exch_rt))+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"),labels = c("NEER", "REER_cpi", "REER_ulc"))+
  labs(title= " Efektivni tečaji, Slovenija",
       caption="Vir: Eurostat; preračuni Umar.")+
  ylab("Dolgoletno povprečje od vstopa v ERMII do zadnjega podatka=100")+
  scale_y_continuous(expand = c(0,0), limits=range(95,110))+
  scale_x_discrete(expand = c(0,0),breaks=c("2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"),
                   labels=c("2008 Q1","2009 Q1","2010 Q1","2011 Q1","2012 Q1","2013 Q1","2014 Q1","2015 Q1","2016 Q1","2017 Q1","2018 Q1","2019 Q1","2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1", "2024 Q1"))+
  theme_UMAR()
 
  p1

  
  p2<- q_EER %>% filter((time >= "2008-01-01")&(exch_rt%in% c("NEER_IC37", "REER_IC37_CPI","REER_IC37_ULCT"))&geo=="SI")%>%select("Indeks2007", "time","exch_rt")%>%
    ggplot(aes(x=factor(time), y=Indeks2007, color=exch_rt))+
    geom_line(size=1, aes(group=exch_rt))+
    scale_color_manual(values=c("#535353","#D99694","#9E001A"),labels = c("NEER", "REER_cpi", "REER_ulc"))+
    labs(title= " Efektivni tečaji, Slovenija",
         caption="Vir: Eurostat; preračuni Umar.")+
    ylab("Indeks 2007=100")+
    scale_y_continuous(expand = c(0,0), limits=range(95,115))+
    scale_x_discrete(expand = c(0,0),breaks=c("2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"),
                     labels=c("2008 Q1","2009 Q1","2010 Q1","2011 Q1","2012 Q1","2013 Q1","2014 Q1","2015 Q1","2016 Q1","2017 Q1","2018 Q1","2019 Q1","2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1", "2024 Q1"))+
    theme_UMAR()
  
  p2
  
p3<- q_EER %>% filter((time >= "2008-01-01")&(exch_rt%in% c("NEER_IC37", "REER_IC37_CPI","REER_IC37_ULCT"))&geo=="SI")%>%select("YOY", "time","exch_rt")%>%
    ggplot(aes(x=factor(time), y=YOY, color=exch_rt))+
    geom_line(size=1, aes(group=exch_rt))+
    scale_color_manual(values=c("#535353","#D99694","#9E001A"),labels = c("NEER", "REER_cpi", "REER_ulc"))+
    labs(title= " Efektivni tečaji, Slovenija",
         caption="Vir: Eurostat; preračuni Umar.")+
    ylab("Medletna sprememba, v %")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0),breaks=c("2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"),
                   labels=c("2008 Q1","2009 Q1","2010 Q1","2011 Q1","2012 Q1","2013 Q1","2014 Q1","2015 Q1","2016 Q1","2017 Q1","2018 Q1","2019 Q1","2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1", "2024 Q1"))+
  theme_UMAR()

p3


p4<- q_EER %>% filter((time >= "2008-01-01") & geo %in% c("SI", "AT", "DE", "FR", "IT") & exch_rt=="REER_IC37_ULCT")%>%select("Indeks2007", "time","geo")%>%
  ggplot(aes(x=factor(time), y=Indeks2007, color=geo))+
  geom_line(size=1, aes(group=geo))+
  scale_color_manual(values=c("#535353","#D99694","#44546A",  "#54A4A3", "#9E001A"))+
  labs(title= " REER ulc",
       caption="Vir: Eurostat; preračuni Umar.")+
  ylab("Indeks 2007=100")+
  scale_y_continuous(expand = c(0,0), limits=range(90,115))+
  scale_x_discrete(expand = c(0,0),breaks=c("2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"),
                   labels=c("2008 Q1","2009 Q1","2010 Q1","2011 Q1","2012 Q1","2013 Q1","2014 Q1","2015 Q1","2016 Q1","2017 Q1","2018 Q1","2019 Q1","2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1", "2024 Q1"))+
  theme_UMAR()
p4 


grid.arrange(p1, p3, p2, p4, ncol=2)
