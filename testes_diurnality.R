library(readr)
library(dplyr)
library(lubridate)
source(file = "diurnality.R")

big = read_delim(file = "/Google Drive/My Drive/Mestrado/[DADOS]/atividade/tratados/big.csv", delim = ";", col_types = "ccdddfd")
big$timestamp = ymd_hms(big$timestamp) # timestamp é POSIXct
big$id = as.factor(big$id) # id é convertido para fator 

big %>% 
    group_by(id, sex) %>% 
    summarise(diurnality = diurnality(datetime = timestamp, activity = odba, interval = 0, lat = -28.8, lon = -66.95))


