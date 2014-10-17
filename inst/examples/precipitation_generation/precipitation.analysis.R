# TODO: Add comment
# 
# Author: ecor
###############################################################################

##### http://science.nature.nps.gov/im/datamgmt/statistics/r/rcourse/session1.cfm
# file diagwl_allstation.R
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
library(RMAWGENplotAlpha)

data(trentino)



### 
#
#TEMPERATURE GENERATION 
#on 
## downloaded from "https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda"
temp_file <- system.file("examples/precipitation_generation/temperature",package="RGENERATEPREC")
prec_file <- system.file("examples/precipitation_generation/precipitation",package="RGENERATEPREC")


temp_file <- paste(temp_file,"results_uncoupled_temperature_generator_P06.rda",sep="/")
prec_file <- paste(prec_file,"output_precipitation.rda",sep="/")

load(temp_file)
load(prec_file)

year_min_sim <- results$year_min
year_max_sim <- results$year_max

Tx_gen <- results$Tx_gen$P06GPCA
Tn_gen <- results$Tn_gen$P06GPCA

station_temp <- names(Tx_gen)

##






###download.file("https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda")
####

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")
origin_sim <- paste(year_min_sim,1,1,sep="-")
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





## TO DO 











## PLOT SIZE

width = 480
height = width
height_unseas=height/2
height_lags=height




wpath <-  "/Users/ecor/R-packages/RGENERATEPREC/inst/examples/precipitation_generation" ####getwd() 
dir.create("RMAWGENplotAlpha_plot")


winter <- c("Dec","Jan","Feb")
spring <- c("Mar","Apr","May")
summer <- c("Jun","Jul","Aug")
autumn <- c("Sep","Oct","Nov")



qqplot=TRUE
if (qqplot) {
	for (it in station) { 
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_prec_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile daily precipitation at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=prec_mes,y=prec_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_prec_RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile daily precipitation at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_unseas <- QuantileQuantilePlot(x=prec_mes,y=prec_gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_unseas)
		dev.off()	
		
		
		
		
		
		
	}
}

plotccgamma=TRUE
return.values=c("nooccurence","occurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
titles <- c("Joint probabilities that station pairs are both dry", "Joint probabilities that station pairs are both wet","Continuity Ratio","Wilks Gaussian Correlation that station pairs are both dry","Binomial Correlation of Precipitetion Occurence")
names(return.values) <- titles

if (plotccgamma) for (it in titles) {
	for (lag in c(0,1)) {
		
		title <- paste(it,"(Lag Y)",sep=" ")
		png <- paste(wpath,'RMAWGENplotAlpha_plot/XXX_lagY_prec_RMAWGEN.png',sep='/')
		
		png <- str_replace(png,"XXX",return.values[it])
		png <- str_replace(png,"Y",lag)
		
		title <- str_replace(title,"Y",lag)
		
		ccgammaplot_out <- ccgammaplot(x=prec_mes,y=prec_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,lag=lag,return.value=return.values[it],valmin=valmin)
		
		png(png,width=width,height=height)
		print(ccgammaplot_out)
		dev.off()	
		
		
	}
}

## Dry and wet Spell 

spell=TRUE
if (spell) for (it in station) {
	
	png_dry <- paste(wpath,'RMAWGENplotAlpha_plot/STATION_dry_spell.png',sep='/')
	png_wet <- paste(wpath,'RMAWGENplotAlpha_plot/STATION_wet_spell.png',sep='/')
	
	png_dry <- str_replace(png_dry,"STATION",it[1])
	png_wet <- str_replace(png_wet,"STATION",it[1])
	
	 
	
	

	
	dry <- spellBoxPlot(x=prec_mes,y=(prec_gen),use.dw.spell=TRUE,station=it,origin=origin,title="Dry Spell Duration",extract="dry",observationIndex="observed",xlab="type",ylab="dry spell [day]",season=TRUE,valmin=valmin)
	wet <- spellBoxPlot(x=prec_mes,y=(prec_gen),use.dw.spell=TRUE,station=it,origin=origin,title="Wet Spell Duration",extract="wet",observationIndex="observed",xlab="type",ylab="wet spell [day]",season=TRUE,valmin=valmin)
	
	
	png(png_dry,width=width,height=height)
	print(dry)
	dev.off()
	
	png(png_wet,width=width,height=height)
	print(wet)
	dev.off()
	
}	
	## QQPLOT 

valmin_spell <- valmin

dw.spell.df.dry.mes <- dw.spell(prec_mes,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)
dw.spell.df.dry.gen <- dw.spell(prec_gen,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)

dw.spell.df.wet.mes <- dw.spell(prec_mes,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)
dw.spell.df.wet.gen <- dw.spell(prec_gen,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)
	
	





dw.spell.qqplot=TRUE
if (dw.spell.qqplot) {
	for (it in station) { 
		
		### dry 
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_dw_spell_dry_prec_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile dry spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_dw_spell_dry_prec__RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile dry spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_unseas <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_unseas)
		dev.off()	
		
		
		### WET 
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_dw_spell_wet_prec_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile wet spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'RMAWGENplotAlpha_plot/qqplot_dw_spell_wet_prec__RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile wet spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_unseas <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_unseas)
		dev.off()	
		
		
		
		
		
	}
}

	###
	
	

##out <- spellBoxPlot(x=prec_mes,y=(prec_gen),use.dw.spell=TRUE,station=names(prec_mes)[2:4],origin=origin,title="Wet Spell Duration",extract="wet",observationIndex="obs",xlab="type",ylab="dry spell [day]",season=TRUE)


