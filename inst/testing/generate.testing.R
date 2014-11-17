library(RGENERATEPREC)

source('/Users/ecor/Dropbox/R-packages/RGENERATEPREC/R/PrecipitationOccurenceMultiSiteModel.R', chdir = TRUE)
source('~/Dropbox/R-packages/RGENERATEPREC/R/predict.PrecipitationOccurenceModel.R', chdir = TRUE)
source('~/Dropbox/R-packages/RGENERATEPREC/R/generate.PrecipitationOccurenceModel.R', chdir = TRUE)

data(trentino)

year_min <- 1961
year_max <- 1990

origin <- paste(year_min,1,1,sep="-")
end <- paste(year_max,12,31,sep="-")

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	acc <- TRUE
	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
	
}

valmin <- 1.0
prec_mes <- prec_mes[,accepted]



Tx_mes <- Tx_mes[,accepted]
Tn_mes <- Tn_mes[,accepted]
prec_occurence_mes <- prec_mes>=valmin

station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
it <- station[2]
vect <- Tx_mes[,it]-Tn_mes[,it]
months <- factor(prec_mes$month)
model <- PrecipitationOccurenceModel(x=prec_mes[,it],exogen=vect,monthly.factor=months,valmin=valmin)

obs <- prec_mes[,it]>=valmin

gen <- generate(model,exogen=vect,monthly.factor=months,n=length(months))


### MultiSite Generation


station <- station[1:4]
exogen <- Tx_mes[,station]-Tn_mes[,station]
##prec_mes <- prec_mes[,station]
names(exogen) <- paste(names(exogen),"exog",sep="_")
months <- factor(prec_mes$month)

#!model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes[,station],exogen=exogen,origin=origin,multisite_type="wilks")


## LOGIT-type Model
model_multisite_logit <- PrecipitationOccurenceMultiSiteModel(x=prec_mes,station=station,exogen=exogen,origin=origin,multisite_type="logit",p=1)


obs_multisite <- prec_mes[,station]>=valmin

#!gen_multisite <- generate(model_multisite,exogen=exogen,origin=origin,end=end)

gen_multisite_logit <- generate(model_multisite_logit,exogen=exogen,origin=origin,end=end)
