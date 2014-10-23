# TODO: Add comment
# 
# Author: ecor
###############################################################################

library(RGENERATEPREC)

data(trentino)

year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]
## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	acc <- TRUE
	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
	
}

valmin <- 1.0
###station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
prec_mes <- prec_mes[,accepted]



Tx_mes <- Tx_mes[,accepted]
Tn_mes <- Tn_mes[,accepted]
prec_occurence_mes <- prec_mes>=valmin

station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]

it <- station[2]
vect <- Tx_mes[,it]-Tn_mes[,it]
months <- factor(prec_mes$month)

model <- glm(prec_occurence_mes[,it] ~ vect+months,family="binomial")
probs <- predict(model,type="response")


plot(Tx_mes[,it]-Tn_mes[,it],probs)




## the dateset is reduced!!!
##prec_mes <- prec_mes[,1:2]
stop()
ndays <- nrow(prec_mes)

station <- "T0129"
valmin <- 1 
prec_mes_prev <- prec_mes[1:(ndays-1),station]
prec_mes_next <- prec_mes[2:ndays,station]>=valmin

### http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
### http://data.princeton.edu/R/glms.html
### http://www.ats.ucla.edu/stat/r/dae/logit/



model <- glm(formula = prec_mes_next ~ prec_mes_prev, family = binomial(logit))





