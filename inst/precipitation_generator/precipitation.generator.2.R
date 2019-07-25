# TODO: Add comment
# 
# Author: ecor
###############################################################################

##### http://science.nature.nps.gov/im/datamgmt/statistics/r/rcourse/session1.cfm
# file precipitation_generator.2.R
# 
# This file creates a weather random generation for daily precipitation
#
# author: Emanuele Cordano on 12-01-2012
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################







rm(list=ls())


library(ggplot2)
library(reshape2)
library(RMAWGEN)
library(stringr)
library(RGENERATEPREC)
library(RGENERATEPRECVis)
library(xtable)

data(trentino)

set.seed(765)

### 
#
#TEMPERATURE GENERATION 
#on 
## downloaded from "https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda"
temp_file <- system.file("examples/precipitation_generation/temperature",package="RGENERATEPREC")
prec_file <- system.file("examples/precipitation_generation/precipitation",package="RGENERATEPREC")


##temp_file <- "/home/ecor/Dropbox/R-packages/RGENERATEPRECVis/inst/article/temperature/results_uncoupled_temperature_generator_P06.rda"
temp_file <- "/home/ecor/Dropbox/R-packages/RGENERATEPREC/inst/temperature/results_uncoupled_temperature_generator_P06.rda"
	###	paste(temp_file,"results_uncoupled_temperature_generator_P06.rda",sep="/")
####prec_file <- paste(prec_file,"output_precipitation.rda",sep="/")

load(temp_file)
####load(prec_file)

year_min_sim <- results$year_min
year_max_sim <- results$year_max

Tx_gen <- results$Tx_gen$P06GPCA
Tn_gen <- results$Tn_gen$P06GPCA

Tx_spline <- results$Tx_spline ###$P06GPCA
Tn_spline <- results$Tn_spine  ####$P06GPCA


station_temp <- names(Tx_gen)

##






###download.file("https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda")
####

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")
origin_sim <- paste(year_min_sim,1,1,sep="-")
end <-  paste(year_max,12,31,sep="-")
end_sim <-  paste(year_max,12,31,sep="-")

valmin <- 1.0

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
##prec_mes <- prec_mes[,1:5]

## SET TEMPERATURE STATION 
station <- names(prec_mes)
station_temp <- station_temp[station_temp %in% station]

Tx_mes <- TEMPERATURE_MIN[period,station_temp]
Tn_mes <- TEMPERATURE_MAX[period,station_temp]
Tx_gen <- Tx_gen[,station_temp]
Tn_gen <- Tx_gen[,station_temp]
Tx_spline <- Tx_spline[,station_temp]
Tn_spline <- Tx_spline[,station_temp]

exogen_var <- Tx_mes-Tx_spline ##-Tn_mes
exogen_var_sim <- Tx_gen-Tx_spline  ##-Tn_gen

names(exogen_var) <- paste(names(exogen_var),"_exog",sep="")
names(exogen_var_sim) <- paste(names(exogen_var_sim),"_exog",sep="")

valmin <- 1.0 


exogen <- normalizeGaussian_severalstations(x=exogen_var, data=exogen_var,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)
exogen_sim <- normalizeGaussian_severalstations(x=exogen_var_sim, data=exogen_var_sim,sample="monthly",extremes=TRUE,origin_x = origin_sim, origin_data = origin_sim)




p <- 1
station <- names(prec_mes)

## Consider only station with measurement ...
#station <- station_temp #####station <- names(prec_mes)

tolerance_wilks=0.005 ###.Machine$double.eps*10^11

#
#


model_multisite_wilks <- PrecipitationOccurenceMultiSiteModel(x=prec_mes,exogen=exogen,origin=origin,p=p,valmin=valmin,tolerance_wilks=tolerance_wilks,multisite_type="wilks",station=station)
model_amount <- PrecipitationAmountModel(prec_mes,station=station,origin=origin,valmin=valmin)



obs_multisite <- prec_mes[,station] ####>=valmin

gen_multisite <- generate(model_multisite_wilks,exogen=exogen_sim,origin=origin_sim,end=end_sim)

obs_mn <- obs_multisite
gen_mn <- generate(model_amount,newdata=gen_multisite,origin_newdata=origin_sim) 
		
		
#####		random.precipitation.values(gen=gen_multisite,obs=obs_multisite,valmin=valmin,station=station)

str(obs_mn)
str(gen_mn)


#obs_mn <- as.data.frame(obs_multisite*5.0)
#gen_mn <- as.data.frame(gen_multisite*5.0)


## PLOTTING RES?ULTS




####wpath <- "/Users/ecor/Dropbox/R-packages/RGENERATEPRECVis/inst/article"
wpath <- '/home/ecor/Dropbox/R-packages/RGENERATEPREC/inst/plot' 

plotccgamma=TRUE ### TRUE 
####return.values=c("nooccurence","occurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
return.values=c("nooccurence","occurence","nooccurence_occurence","occurence_nooccurence","continuity_ratio","probability_continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
titles_long <- c("Joint probabilities that station pairs are both dry","Joint probabilities that station pairs are both wet","Joint probabilities that one station is dry and the other one is wet","Joint probabilities that one station is wet and the other one is dry",
		"Continuity Ratio","Occurrence Continuity Ratio","Wilks Gaussian Correlation that station pairs are both dry"
		,"Binomial Correlation of Precipitation Occurence")           
titles <- titles_long

titles[1] <- "Dry-Dry Probability"
titles[2] <- "Wet-Wet Probability"
titles[3] <- "Dry-Wet Probability"
titles[4] <- "Wet-Dry Probability"

titles[7] <- "Wilks Correltion"
titles[8] <- "Binomial Correltion"

##titles <- titles_long

names(return.values) <- titles 

width = 180 ###120 ####200 ###480
height = 480 ###4*width
if (plotccgamma) for (it in titles) {
		for (lag in c(0,1)) {
			
			title <- paste(it,"(Lag Y)",sep=" ")
		
			png <- paste(wpath,'plot2/XXX_lagY_prec_wilks_RMAWGEN.png',sep='/')
			
			png <- str_replace(png,"XXX",return.values[it])
			png <- str_replace(png,"Y",lag)
			
			title <- str_replace(title,"Y",lag)
			title <- it
			ccgammaplot_out <- ccgammaplot(x=obs_mn,y=gen_mn,corx=NULL,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,lag=lag,return.value=return.values[it],valmin=valmin,tolerance=tolerance_wilks)
			
			png(png,width=width,height=height)
			print(ccgammaplot_out)
			dev.off()
			
			
		}
	}


## QQPLOT 



## QQPLOT 

valmin_spell <- valmin

dw.spell.df.dry.mes <- dw.spell(obs_mn,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)
dw.spell.df.dry.gen <- dw.spell(gen_mn,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)

dw.spell.df.wet.mes <- dw.spell(obs_mn,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)
dw.spell.df.wet.gen <- dw.spell(gen_mn,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)


width = 480
height = width
height_unseas=height/2
height_lags=height

stationlist <- as.list(station)
names(stationlist) <- station
stationlist$added1 <- c("T0083","T0090","T0129")



dw.spell.qqplot=TRUE
if (dw.spell.qqplot) {
	
	dw.spell.test.wet <- list()
	dw.spell.test.dry <- list()
	
	for (it in stationlist) { 
		
		### dry 
		
		png <- paste(wpath,'plot2/qqplot_dw_spell_dry_prec_wilks_RMAWGEN_XXX.png',sep='/')  
		title_long <- "Quantile-Quantile dry spell length [days] at XXX"		
		title <- "Quantile-Quantile dry spell length [days]"
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'plot2/qqplot_dw_spell_dry_prec_wilks__RMAWGEN_XXX_unseas.png',sep='/')  
		title_long <- "Quantile-Quantile dry spell length [days] at XXX"		
		title <- "Quantile-Quantile dry spell length [days]"	
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_prec_wilks_unseas <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_wilks_unseas)
		dev.off()	
		
		
		### WET 
		
		png <- paste(wpath,'plot2/qqplot_dw_spell_wet_prec_wilks_RMAWGEN_XXX.png',sep='/')  
		title_long  <- "Quantile-Quantile wet spell length [days] at XXX"		
		title <- "Quantile-Quantile wet spell length [days]"	
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'plot2/qqplot_dw_spell_wet_prec_wilks__RMAWGEN_XXX_unseas.png',sep='/')  
		title_long <- "Quantile-Quantile wet spell length [days] at XXX"		
		title      <- "Quantile-Quantile wet spell length [days]"	
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_prec_wilks_unseas <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_wilks_unseas)
		dev.off()	
		
		
		if (length(it)==1) dw.spell.test.dry[[it]] <- function.test(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,sampe="monthly",station=it)
		if (length(it)==1) dw.spell.test.wet[[it]] <- function.test(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,sampe="monthly",station=it)
		
	}
}


#### 
nwetdays_obs <- nwetdays(obs_mn,origin=origin,valmin=valmin,station=station)
nwetdays_gen <- nwetdays(gen_mn,origin=origin,valmin=valmin,station=station)

nwetdayplot=TRUE
nwetday.list.test <- list()

if (nwetdayplot==TRUE) for (it in stationlist) {
		
		png <- paste(wpath,'plot2/qqplot_nwetdays_prec_wiks_RMAWGEN_XXX.png',sep='/')  
		title_long <- "Quantile-Quantile number of wet days per month [days] at XXX"		
		title <- "Quantile-Quantile number of wet days per month [days]"
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_nwet <- QuantileQuantilePlot(x=nwetdays_obs,y=nwetdays_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_nwet)
		dev.off()	
		
		
		if (length(it)==1) nwetday.list.test[[it]] <- function.test(x=nwetdays_obs,y=nwetdays_gen,sampe="monthly",station=it)
	}

qqplot_prec=TRUE
precvalue.list.test <- list()
if (qqplot_prec==TRUE) for (it in stationlist) {
		
	
		png <- paste(wpath,'plot2/qqplot_precipitationdepth_prec_wilks_RMAWGEN_XXX.png',sep='/')  
		title_long <- "Quantile-Quantile Daily Precipitation Depth [mm] at XXX"		
		title <- "Quantile-Quantile Daily Precipitation Depth [mm]"
		png <- str_replace(png,"XXX",paste(it,collapse="_"))
		title <- str_replace(title,"XXX",paste(it,collapse=","))
		qqplot_prec <- QuantileQuantilePlot(x=obs_mn,y=gen_mn,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		if (length(it)==1) precvalue.list.test[[it]] <- function.test(x=obs_mn,y=gen_mn,sampe="monthly",station=it,valmin=valmin,origin_x=origin,origin_y=origin_sim)
	}





test.list <- c("precvalue.list.test","nwetday.list.test","dw.spell.test.dry","dw.spell.test.wet")


#####
help(xtable)
#####

nmonth <- 12
pvalue_ks <- list()
for (it in test.list) {
	
	out_ <- get(it)
	out <- array(NA,c(length(out_),nmonth))
	
	for (r in 1:nrow(out)) {
		for (c in 1:ncol(out)) { 
		
			
			
			out[r,c] <- out_[[r]][[c]]$ks.test$p.value
			
			
			
		}
		
		
	}
	rownames(out) <- names(out_)
	colnames(out) <- sprintf("%02d",1:ncol(out))
	
	
	
	pvalue_ks[[it]] <- out
	
}


test.list <- c(test.list,"pvalue_ks")
file <- '/home/ecor/Dropbox/R-packages/RGENERATEPREC/inst/plot/test.rda'   ### "/Users/ecor/Dropbox/R-packages/RGENERATEPRECVis/inst/article/tests/test.rda"
save(list=test.list,file=file)



