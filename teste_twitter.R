# https://rtweet.info/index.html
library(rtweet)
library(ggplot2)
install.packages("maps")
library(maps)
### acesso API
token <- create_token(
	app = "rtweet_activity",
	consumer_key = "2rWMxT3yfV9yhxNwQ0Yydb6Ih",
	consumer_secret = "juBMOABzVkOV1lHCMuDi4UUyvlujtv4EarHgr9zV1lbrAMEEbl",
	access_token = "707319823768932352-mZJDoQiZ7wrePjDimltQFf4QDGYNaOY",
	access_secret = "bXY0MwyHV5Cf8zCJnxxtVwPRq46hsZsen2tHPMsc42EpX")

rt <- stream_tweets("")

#### get timelines
bozo = get_timelines(c("jairbolsonaro"), n = 1000)
head(bozo)


## plot the frequency of tweets for each user over time
bozo %>%
	ts_plot("days", trim = 1L) +
	ggplot2::geom_point() +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		legend.title = ggplot2::element_blank(),
		legend.position = "bottom",
		plot.title = ggplot2::element_text(face = "bold")) +
	ggplot2::labs(
		x = NULL, y = NULL,
		title = "Frequency of Twitter statuses posted by news organization",
		subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
		caption = "\nSource: Data collected from Twitter's REST API via rtweet"
	)

grep("Summer", rt)
rt[6,5]



## testando listas
n = c(2, 3, 5) 
s = c("aa", "bb", "cc", "dd", "ee") 
b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x = list(n, s, b, 3)   # x contains copies of n, s, b

x[[2]][2]
