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

library(RGENERATEPREC)


data(trentino)



### 
#
#TEMPERATURE GENERATION 
#on 
## downloaded from "https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda"
temp_file <- system.file("examples/precipitation_generation/temperature",package="RGENERATEPREC")
prec_file <- system.file("examples/precipitation_generation/precipitation",package="RGENERATEPREC")
prec_file <- '/Users/ecor/R-packages/RGENERATEPREC/inst/examples/precipitation_generation/precipitation' 


temp_file <- paste(temp_file,"results_uncoupled_temperature_generator_P06.rda",sep="/")
prec_file <- paste(prec_file,"output_multiprecipitation.rda",sep="/")

load(temp_file)


year_min_sim <- results$year_min
year_max_sim <- results$year_max

Tx_gen <- results$Tx_gen$P06GPCA
Tn_gen <- results$Tn_gen$P06GPCA

Tx_spline <- results$Tx_spline
Tn_spline <- results$Tn_spline

station_temp <- names(Tx_gen)

##






###download.file("https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda")
####

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")
origin_sim <- paste(year_min_sim,1,1,sep="-")

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
Tn_spline <- Tn_spline[,station_temp]


Tm_mes <- (Tx_mes+Tn_mes)/2
Tm_gen <- (Tx_mes+Tn_mes)/2
Tm_spline <- (Tx_spline+Tn_spline)/2
dT_mes <- Tx_mes-Tm_mes
dT_gen <- Tx_gen-Tn_gen

names(Tx_mes) <- paste("Tx",names(Tx_mes),sep="_")
names(Tn_mes) <- paste("Tn",names(Tn_mes),sep="_")
names(Tm_mes) <- paste("Tm",names(Tm_mes),sep="_")
names(dT_mes) <- paste("dT",names(dT_mes),sep="_")

names(Tx_gen) <- paste("Tx",names(Tx_gen),sep="_")
names(Tn_gen) <- paste("Tn",names(Tn_gen),sep="_")
names(Tm_gen) <- paste("Tm",names(Tm_gen),sep="_")
names(dT_gen) <- paste("dT",names(dT_gen),sep="_")

names(Tx_spline) <- paste("Tx",names(Tx_spline),sep="_")
names(Tn_spline) <- paste("Tn",names(Tn_spline),sep="_")
names(Tm_spline) <- paste("Tm",names(Tm_spline),sep="_")


exogen_var <- cbind(Tm_mes-Tm_spline) ##,Tx_mes-Tn_mes) ###  cbind(Tx_mes,Tn_mes)-cbind(Tx_spline,Tn_spline)
exogen_var_sim <- cbind(Tm_gen-Tm_spline) ##,Tx_gen-Tn_gen) ##cbind(Tx_gen,Tn_gen)-cbind(Tx_spline,Tn_spline)

exogen  <- normalizeGaussian_severalstations(x=exogen_var, data=exogen_var,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)
exogen_sim  <- normalizeGaussian_severalstations(x=exogen_var_sim, data=exogen_var,sample="monthly",extremes=TRUE,origin_x = origin_sim, origin_data = origin_sim)


## END SET TEMPERATURE STATION

valmin <- 0.5
prec_mes_gaussWilks <- WilksGaussianization(x=prec_mes, data=prec_mes,valmin=valmin,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,force.precipitation.value="both")

prec_mes_ginv  <- normalizeGaussian_severalstations(x=prec_mes_gaussWilks, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,inverse=TRUE)
ccgamma <- CCGamma(data=prec_mes, lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)
##ccgamma_wilks <- cor(prec_mes_gaussWilks,use="pairwise.complete.obs")

##
##


##
##

prec_mes_gaussWilks[is.na(prec_mes_gaussWilks)] <- -1
##VARselect(prec_mes_gaussWilks)

gauss_prec_var <- VAR(prec_mes_gaussWilks,exogen=exogen,type="none",p=3)
gauss_prec_gpcavar <- getVARmodel(data=prec_mes_gaussWilks,suffix=NULL,p=1,n_GPCA_iteration=5,n_GPCA_iteration_residuals=5,exogen=exogen)
## Precipitation Stochastic Generation
set.seed(1223)
generation_names <- sprintf("GEN%02d",1:10)

prec_gen_gaussWilks <- list()
prec_gen <- list()
for (it in generation_names) {
	
	prec_gen_gaussWilks[[it]] <- generate(x=gauss_prec_gpcavar,exogen=exogen_sim,n=nrow(prec_mes_gaussWilks),names=names(prec_mes_gaussWilks))
	prec_gen[[it]]  <- normalizeGaussian_severalstations(x=prec_gen_gaussWilks[[it]], data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin_sim, origin_data = origin_sim,inverse=TRUE)

}


ccgamma_wilks <- CCGamma(data=prec_gen[[2]], lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)


plot(ccgamma,ccgamma_wilks)
abline(0,1)

save(prec_mes_gaussWilks,prec_gen_gaussWilks,prec_gen,gauss_prec_var,origin_sim,file=prec_file)
