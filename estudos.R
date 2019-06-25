# estudos 

# https://stackoverflow.com/questions/39869515/minutely-mean-value-in-r

# https://stackoverflow.com/questions/22203493/aggregate-1-minute-data-into-5-minute-average-data

Função cut.POSIXt()
levels(cut.POSIXt(x = acelerometro$data.hora, breaks = "5 day"))


brk = "5 day"
df2 <- transform(acelerometro, timeGrp = cut(as.POSIXct(acelerometro$data.hora, format = "%H:%M:%S"), breaks = brk))

x = data.hora
b = aggregate(ODBA~data.hora, acelerometro, mean)
