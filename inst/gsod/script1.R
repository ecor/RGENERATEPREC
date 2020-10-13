NULL

#
#
#

rm(list=ls())


library(GSODR)

wa_iso3 <- c("BEN","BFA","CPV","CPV","CIV","GMB","GHA","GIN","GNB","LBR","MLI","MRT","NER","NGA","SEN","SLE","TGO","CMR","TCD","CAF")
years <- 1949:2018
# Download data for Australia from 2010 to 2011
out <- get_GSOD(years = years, country = wa_iso3)