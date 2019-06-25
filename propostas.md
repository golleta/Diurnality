# Proposta 2: Índice de diurnalidade
## Contextualização
Em estudos de padrões de atividade e ritmos biológicos o índice de diurnalidade é usado para medir o quanto um animal está noturno ou diurno. Esse índice é particularmente útil em estudos que avaliam a alteração do padrão de atividade sazonal ou de animais que mudam de noturnos para diurnos em certas situações experimentais. O Índice de diurnalidade é uma proporção entre a quantidade de atividade exibida durante as horas de claro e a atividade durante a fase de escuro. O índice é simétrico ao redor de 0 e pode variar de -1 até 1, onde 1 representa um animal completamente diurno e -1 um animal completamente noturno. Esse índice é calculado pela seguinte equação:

O objetivo da função é calcular o índice de diurnalidade do intervalo de dias fornecido pelo usuário ao longo do conjunto de dados.

## Entrada
diurnality (datetime, activity, interval = 1, sunrise = NULL, sunset = NULL)

- datetime = um vetor de datahora
- activity =  um vetor de numérico
- interval = Número inteiro de dias para calculo do índice.
- sunrise = horário de nascer do sol em formato HH:MM.
- sunset =  horário de por do sol em formato HH:MM.

	- Receber o horário do nascer e do por do sol pelo usuário funcionará bem em casos onde dados não se estendam por um tempo muito longo. Porém, caso o volume de dados seja grande e se estenda, por exemplo, por um ano é necessário calcular a duração do dia a medida que o índice é calculado.
	- Nesse caso pensei em usar a função _getSunlightTimes_ do pacote _suncalc_ para calcular a duração do dia. Dessa maneira, eu adicionaria mais três argumentos na função: **latitude, longitude e timezone**. Esses são os mesmos argumentos usados pela função _getSunlightTimes_. (Minha dúvida é se essa é uma boa prática)

## Verificando parametros
1. datahora = POSIXct
2. activity = inteiro
3. interval = inteiro
4. sunrise = null
5. sunset = null

## Pseudocódigo
1. Verificar argumentos
2. Inicializar variaveis
3. ni = número intervalos (total de dias/intervalo)
4. For de i de 1:ni
	1. Atribuir dados que estão dentro do intervalo a um novo vetor
	2. nr = numero de registros dentro do intervalo
	3. Sunrise = _getSunlightTimes()$sunrise_
	4. Sunset = _getSunlightTimes$sunset_
		1. For j de 1:nr
			1. Horário do registro[j] está no intervalo entre sunrise e sunset?
				1. Se sim day.act = day.act + atividade do registro[j]
				2. Se não night.act = night.act + atividade do registro[j]
		1. Fim do loop
		2. Calcular índice do intervalo diurnality[i] = (day.act - night.act)/(day.act + night.act)
1. Fim do Loop
2. Plot diurnality
1. Return diurnality

## Saída
- Um vetor com os índices calculados
- Um gráfico de pontos

---

# Proposta 2: Plotando Tweets

O twitter é, infelizmente, o meio de comunicação oficial de Bolsonaro. Acompanhar as postagens do presidente, no entanto, é um exercício de paciência. Assim, minha segunda proposta é fazer uma função que receba o nome de um usuário qualquer do twitter e retorne alguns gráficos que sumarizem o número de tweets, RTs e curtidas por dia. Além disso, permitir ao usuário fornecer uma ou mais palavras chaves para fazer uma busca específica dentro dos twitts. A idéia aqui é reproduzir algo como feito nesse artigo do Nexo porém para uso contínuo.

## Entrada
plot_tweet(user, n = 1000, keywords = NULL)

user = nome do usuário no twitter
keywords = palavras chaves a serem buscadas nos tweets do usário
n = número de tweets a serem recuperados, dever ser menor do que 3200

## Pseudocódigo
Verificar argumentos
	''n'' não pode ser maior do que 3200, limite da API.
Conectar com API do twitter usando o pacote ''rtweet''
Fazer chamada para recuperar dados do usuário desejado
Atribui o resultado da busca à variável ''tweets''
dia de RTs por tweet e médias de curtidas por tweet
Plot de Quantidade de tweets, RTs e curtidas em formato de calendário usando ''ggplot2''

Se lenght(keywords) > 1 então:
Cria uma lista ''tweets.summary'' que recebe na posição [[1]] os valores de média de tweets por dia, média de RTs por tweet e médias de curtidas por tweet

For i de 1:lenght(keywords)
tweets.keywords = grep(keyword[i], tweets)
Plot de quantidade de tweets por palavra chave usando ''ggplot2''
tweets.summary[[i]] recebe os valores da média de tweets com a palavra chave por dia, média de RTs por tweet e médias de curtidas por tweet
Fim do for

Se não:
Cria uma vetor ''tweets.summary'' que recebe os valores de média de tweets por dia, média de RTs por tweet e média de curtidas por tweet.

## Saída
Uma lista ou vetor com os valores médios de tweets por dia, RTs por tweet e curtidas por tweet
Gráficos do tipo heatmap com a quantidade de tweets, RTs e curtidas por dia

# Alguns links:

http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Calendar%20Heat%20Map
http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Calendar%20Heat%20Map
