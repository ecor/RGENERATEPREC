# TODO: Add comment
# 
# Author: ecor
###############################################################################

library(RGENERATEPREC)

data(trentino)

year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
prec_mes <- PRECIPITATION[period,station]

## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
}

prec_mes <- prec_mes[,accepted]
## the dateset is reduced!!!
##prec_mes <- prec_mes[,1:2]

ndays <- nrow(prec_mes)

station <- "T0129"
valmin <- 1 
prec_mes_prev <- prec_mes[1:(ndays-1),station]
prec_mes_next <- prec_mes[2:ndays,station]>=valmin


### http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
### http://data.princeton.edu/R/glms.html
### http://www.ats.ucla.edu/stat/r/dae/logit.htm/

model <- glm(formula = prec_mes_next ~ prec_mes_prev, family = "binomial")

fitted(model)-predict(model,type="response")
# Logistic Regression
# where F is a binary factor and 
# x1-x3 are continuous predictors 
#fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())
#summary(fit) # display results
#confint(fit) # 95% CI for the coefficients
#exp(coef(fit)) # exponentiated coefficients
#exp(confint(fit)) # 95% CI for exponentiated coefficients
#predict(fit, type="response") # predicted values
#residuals(fit, type="deviance") # residuals

### http://www.statmethods.net/advstats/glm.html




plot(prec_mes_next,residuals(model,type="response")+predict(model,type="response"))
