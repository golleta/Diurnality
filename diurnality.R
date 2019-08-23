##################################
# Calculo do indice de diunalidade
# Jefferson Silva
###################################

diurnality = function(datetime, activity, interval = 0, lat = NULL, lon = NULL, sunrise = NULL, sunset = NULL, graph = FALSE){
	
	# checa se pacotes necessários estão instalados
	require(lubridate)
	# checa se pacotes necessários estão instalados
	if (!require(maptools)){
		# Instala pacotes caso não estejam instalados
		install.packages("maptools")
	}else
	{
		# carrega pacotes necessários
		# suncalc é usado para calcular a duração do dia em determinado dia do ano dado as coordenadas do local
		require(maptools)
	}
	#####################
	# Verifica argumentos
	#####################
	
	#### Datetime deve ser do tipo POSIXct para evitar erros futuros
	if(!inherits(datetime, "POSIXct")){
		# caso não seja a função é parada
		stop("Argumento 'datetime' deve ser da classe POSIXct")
	}
	
	# Intervalo para calculo do indice deve ser > 1
	if(interval<0){
	  stop("Argumento 'interval' deve ser positivo")
	}
	
	if(!is.numeric(activity)){
		stop("Argumento 'activity' deve ser numérico.")
	}
	
	if(length(activity) != length(datetime)){
		stop("Argumentos 'datetime' e 'activity'devem ter o mesmo tamanho.")
	}
	
	#### Devem ser preenchido (sunrise e sunset) ou então (lat, lon)
	if(is.null(sunrise) | is.null(sunset)){
		if(is.null(lat) | is.null(lon) ){
			# Nenhum dos conjuntos de argumentos foi fornecido
			stop("É necessário fornecer manualmente o valor de nascer e pôr do sol ou as coordenadas do local.")
		}
		else{
			# caso apenas lat lon sejam fornecidas
			cat(">>> A duração do dia será automaticamente calculada usando as coordenadas fornecidas.\n")
			# atrbui TRUE para uma variavel indicadora, o que significa que a duração do dia seá calculada usando o pacote suncalc
			coord = TRUE
		}
	}
	else{
		if(is.null(lat) | is.null(lon)){
			# checa se sunrise e sunset estão no formato correto de HH:MM
			if ( is.na(as.POSIXct(paste(Sys.Date(), sunset), format = "%Y-%m-%d %H:%M")) | is.na(as.POSIXct(paste(Sys.Date(), sunrise), format = "%Y-%m-%d %H:%M"))) {
				stop("'Sunrise' ou 'Sunset' não estão no formato HH:MM.")
			}
			else{
				# caso apenas sunrise e sunset sejam fornecidos
				cat(">>> A duração do dia será feita com base no horário de nascer e pôr do sol fornecidos.")
			}
			# atrbui FALSE, indicando que será usado os valores de sunrise e sunset fornecidos pelo usuário
			coord = FALSE
		}
		else{
			# Os dois conjuntos de argumentos foram fornecidos
			stop("Apenas um conjunto de argumentos deve ser fornecido entre horário de nascer e pôr do sol e as coordenadas.")
		}
	}
	
	
	####################
	## calcula indice ##
	####################
	
	# Combina dados de entrada em um dataframe, o que facilita a manipulação.
	df = data.frame(datetime, activity)
	# omite NAs
	df = na.omit(df)
	
	# Usar coordenadas para calculo da duração do dia?
	if (coord == FALSE){
		## Esse bloco de código cria uma nova coluna no dataframe
		## A nova coluna 'daylight' indica que o registro correspondente aquela linha foi realizado durante o dia
		# Para cada linha extrai somente o valor da data, descartando as horas
		date = as_date(df$datetime)
		# concatena a data correspondete daquela linha com o horário de nascer e pôr do sol
		sunrise = ymd_hm(paste(date, sunrise))
		sunset = ymd_hm(paste(date, sunset))
		# Verifica se o registro foi feito entre as horas de nascer e por do sol e adiciona a nova coluna 'daylight' ao dataframe
		df$daylight = ifelse(test = df$datetime >= sunrise & df$datetime <= sunset, yes = TRUE, no = FALSE)
		
	}	
	else{
		## O pacote 'maptools' é usado para conseguir a duração do dia automaticamente
		# As coordenadas devem ser convertidas em uma matriz
		crds = matrix(c(lon, lat), nrow = 1)
		
		# calcula horarios de inicio dos crepusulos (dawn e dusk)
		# Qual angulo solar usar? https://www.timeanddate.com/astronomy/different-types-twilight.html
		dawn <- crepuscule(crds = crds, dateTime = df$datetime, solarDep = 6, direction = "dawn", POSIXct.out=TRUE)$time
		dusk <- crepuscule(crds = crds, dateTime = df$datetime, solarDep = 6, direction = "dusk", POSIXct.out=TRUE)$time
		
		# Verifica se df está entre as horas de nascer e por do sol e adiciona a nova coluna 'daylight' ao dataframe.
		df$daylight = ifelse(test = df$datetime  >= dawn & df$datetime  <= dusk, yes = TRUE, no = FALSE)
	}
	
	## Se o intervalo é maior ou igual a 0 o indice é calculado para toda atividade do dataset fornecido
	## Caso o intervalo seja maior ou igual a 1 o indice é calculado por grupo de dias
	if (interval == 0){
		# indexa valores de atividade que acontecem durante o dia
		actv.day = df$activity[df$daylight]
		# indexa valores de atividade que acontecem durante a noite
		actv.night = df$activity[!df$daylight]
		# somatória da atividade diurna
		actv.day = sum(actv.day)
		# somatória da atividade noturna
		actv.night = sum(actv.night)
		# calculo do indice de diurnalidade (Hoogenboom, 1984)
		d.index = (actv.day - actv.night)/(actv.day + actv.night)
	}
	else{
		# Cria uma string de acordo com o intervalo de dias fornecido nos argumentos
		b = paste(as.character(interval),"days")
		# Cria um fator que corresponde ao intervalo de cada uma das linhas do df.
		cuts = cut(x = df$datetime, breaks = b)
		#  Cria uma lista dividida por intervalos
		df.list = split(x = df, f = cuts)
		# Separa atividade que acontece durante dia ou noite e calcula o indice de diurnalidade (Hoogenboom, 1984)
		d.index = sapply(df.list, 
						 function(x){
						 	# indexa valores de atividade que acontecem durante o dia
						 	actv.day = x$activity[x$daylight]
						 	# indexa valores de atividade que acontecem durante a noite
						 	actv.night = x$activity[!x$daylight]
						 	# somatória da atividade diurna
						 	actv.day = sum(actv.day)
						 	# somatória da atividade noturna
						 	actv.night = sum(actv.night)
						 	# calculo do indice de diurnalidade (Hoogenboom, 1984)
						 	d = (actv.day - actv.night)/(actv.day + actv.night)
						 	return(d)
						 })
		d.index = data.frame(date = names(d.index), diurnality = d.index, row.names = NULL)
	}
	
	
	##########
	## plot ##
	##########
	if(graph){
		# Prepara variaveis para criar retangulo e label do eixo X
		n = length(d.index)
		date.axis = strptime(names(d.index),"%Y-%m-%d")
		
		# Define como exibir as legendas
		if(interval >= 30){
			# Para intervalos maiores do que 30 dias exibe o nome do mês e o ano.
			date.axis = format(date.axis, "%b\n%Y")
		}
		else{
			# Para intervalos menores do que 30 dias exibe dia e nome do mês.
			date.axis = format(date.axis, "%d\n%b")
		}
		
		# prepara paramentros gráficos
		par(pch=16,  tcl=-0.3,  bty="l", cex.axis = 0.9, las = 1, cex.lab=1, mar=c(4,4,2,2))
		# plota pontos
		plot(d.index, 
			 main = "", 
			 xlab = "",
			 xaxt = "n",
			 ylab = "Diurnality Index",
			 ylim = c(-1,1)
		)
		# adiciona eixo X
		axis(1, at=1:n, labels=date.axis, mgp = c(3, 1.5, 0))
		# adiciona retangulo cinza na parte noturna do indice
		rect(xleft = c(-1,-1), ybottom = c(-2,-2), xright = c(n+2,n+2), ytop = c(0,0), col = rgb(0,0,0,0.009), lty=3, border = NA)
		# conecta pontos
		lines(x = 1:n, y = d.index, col = rgb(0,0,0,0.2), type = "b")
		# adiciona linha horizontal
		abline(h = 0, lty = 3, col = rgb(0,0,0,0.5))
	}
	
	############
	## return ##
	############
	return(d.index)
	
} #FIM DIURNALITY
