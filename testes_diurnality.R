library(readr)
library(dplyr)
library(lubridate)
source(file = "diurnality.R")

big = read_delim(file = "/Google Drive/My Drive/Mestrado/[DADOS]/atividade/tratados/big.csv", delim = ";", col_types = "ccdddfd")
big$timestamp = ymd_hms(big$timestamp) # timestamp é POSIXct
big$id = as.factor(big$id) # id é convertido para fator 

big %>% 
    group_by(id, sex) %>% 
    summarise(diurnality = diurnality(datetime = timestamp, activity = odba, interval = 0, lat = -28.8, lon = -66.95, mav.window = 0, as.percentage = FALSE))

debug(diurnality)
diurnality(datetime = big.subset$timestamp, activity = big.subset$odba, interval = 0, lat = -28.8, lon = -66.95, as.percentage = FALSE )



####
####
####
####


# calcula o valor de atividade médio do animal
actv.mean = mean(df$activity, na.rm = T)

# Checa valor da atividade conta média e retorna um vetor lógico. TRUE = com atividade.
df$actv.logical = ifelse(test = df$activity >= actv.mean, yes = TRUE, no = FALSE)

df = df[as_date(df$timestamp) == as_date("2019-07-10 "),]

teste = circular(df$actv.logical, units = "hours", template = "clock24")
teste.mean = mean.circular(teste,na.rm = T)

plot.circular(teste.mean)

# indexa valores de atividade que acontecem durante o dia
actv.day = df$actv.logical[df$daylight]
# indexa valores de atividade que acontecem durante a noite
actv.night = df$actv.logical[!df$daylight]