# TODO: Add comment
# 
# Author: ecor
###############################################################################

##### http://science.nature.nps.gov/im/datamgmt/statistics/r/rcourse/session1.cfm
# file precipitation.generator.example.R
# 
# This file creates a weather random generation for daily precipitation
#
# author: Emanuele Cordano on 10-11-2015
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


temp_file <- "/Users/ecor/Dropbox/R-packages/RGENERATEPRECVis/inst/article/temperature/results_uncoupled_temperature_generator_P06.rda"
		
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


## GENERATON OF PRECIPITATION AMOUNT 


obs_mn <- obs_multisite
gen_mn <- generate(model_amount,newdata=gen_multisite,origin_newdata=origin_sim) 
		
		
#####		random.precipitation.values(gen=gen_multisite,obs=obs_multisite,valmin=valmin,station=station)

str(obs_mn)
str(gen_mn)

