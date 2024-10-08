# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#'
#' It calculates dry/wet spell duration. 
#' 
#' 
#' @param data data frame R object containing daily precipitation time series for several gauges (one gauge time series per column). 
#' @param valmin threshold precipitation value [mm] for wet/dry day indicator.
#' @param origin character string \code{"yyyy-mm-dd"} indicated the date of the first row of \code{"data"}. 
#' @param extract string character referred to the state to be extracted, eg. \code{"dry"} or \code{"wet"}
#' @param month integer vectors containing the considered months. Default is \code{1:12} (all the year). 
#' @param melting.df logical value. If it \code{TRUE} the output is melted into a data frame. Default is \code{FALSE}.
#' @param from.start logical value. If is \code{TRUE} the spell is referenced to its first day, if it is \code{FALSE} (default) the spell is referenced to its last date.
#' @param only.inner logical value. It is used in case \code{extract} is not \code{NULL}, if the value is \code{TRUE}, it extracts dry/wet spells completely inside the selected \code{month} period. Default is \code{FALSE}. 
#' 
#' @export
#'
#' @importFrom RMAWGEN adddate
#' @importFrom RMAWGEN continuity_ratio
#' @importFrom lubridate day month year
#' @return Function returns a list of data frames containing the spell length expressed in days
#' 
#' @examples  
#' 
#' 
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
#' 
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' prec_mes <- PRECIPITATION[period,station]  
#' 
#' ## removing nonworking stations (e.g. time series with NA)
#' accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#' for (it in names(prec_mes)) {
#' 		 accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
#' }
#'
#' prec_mes <- prec_mes[,accepted]
#' ## the dateset is reduced!!! 
#' prec_mes <- prec_mes[,1:3]
#' 
#' origin <- paste(year_min,1,1,sep="-")
#' dw_spell <- dw.spell(prec_mes,origin=origin)
#' dw_spell_dry <- dw.spell(prec_mes,origin=origin,extract="dry")
#' 
#' hist(dw_spell_dry$T0001$spell_length)
#' 
#' 
#' ## Single Gauging Station
#' 
#' prec_mes <- prec_mes[,1]
#' 
#' origin <- paste(year_min,1,1,sep="-")
#' dw_spell <- dw.spell(prec_mes,origin=origin)
#' dw_spell_dry <- dw.spell(prec_mes,origin=origin,extract="dry")
#' dw_spell_dry_start <- dw.spell(prec_mes,origin=origin,extract="dry",
#' 	month=5:8,from.start=TRUE) ## dry spell 
#' dw_spell_dry_start_2 <- dw.spell(prec_mes,origin=origin,extract="dry",
#' month=5:8,from.start=TRUE,only.inner=TRUE) ## dry spell 
#' ## is referenced to the first day instead of the latest one as default. 
#' 
#' hist(dw_spell_dry[[1]]$spell_length)
#' 
#' 





dw.spell <- function(data,valmin=0.5,origin="1961-1-1",extract=NULL,month=1:12,melting.df=FALSE,from.start=FALSE,only.inner=FALSE) {
	
	
	out <- list()
	
	data <- adddate(data,origin=origin)
	nrdata <- nrow(data)
	
	ignore.date <- !(names(data) %in% c("year","month","day"))
	###data <- data[,!(names(data) %in% c("year","month","day"))]
	
	for (c in 1:ncol(as.data.frame(data[,ignore.date]))){
		
		val <- as.vector(as.data.frame(data[,ignore.date])[,c])
		
		spell_state <- array("dry",length(val))
		
		spell_length <- array(1,length(val))
	###	spellstart <- array(FALSE,length(val))
		spell_end <- array(FALSE,length(val))
		spell_state[which(is.na(val))] <- "na"
		spell_state[which(val>valmin)] <- "wet"
	
		
		spell_end[1] <- TRUE
		
		for (i in 2:length(spell_state)) {
			
			if (spell_state[i]==spell_state[i-1]) {
				
				spell_end[i] <- TRUE 
				spell_end[i-1] <- FALSE
				spell_length[i] <- spell_length[i]+spell_length[i-1]
			} else{ 
			
				spell_end[i] <- TRUE 
				spell_length[i] <- 1 
			
			}
			
			
		}
	
		spell_length <- spell_length[which(spell_end)]
		spell_state <- spell_state[which(spell_end)]
		
		temp <- data[which(spell_end),!ignore.date]
		
		temp$spell_length <- spell_length
		temp$spell_state <- spell_state
		temp$end_date <- as.Date(paste(temp$year,temp$month,temp$day,sep="-"))
		temp$start_date <- temp$end_date-lubridate::days(temp$spell_length-1)
		
		### FROM START ## mod EC 20191016
		if (from.start==TRUE) {  ## MOD EC 202240508  ## MOD EC 20201007
			
		 ##temp$end_date <- as.Date(paste(temp$year,temp$month,temp$day,sep="-"))
		 ##temp$start_date <- temp$end_date-temp$spell_length+1
		 temp$day <- lubridate::day(temp$start_date) ## as.numeric(as.character(temp$start_date,format="%d"))
		 temp$month <- lubridate::month(temp$start_date) ## as.numeric(as.character(temp$start_date,format="%m"))
		 temp$year <- lubridate::year(temp$start_date) ##as.numeric(as.character(temp$start_date,format="%Y"))
		 
		}		
		out[[c]] <- temp 
		
	}
	
	names(out) <- names(as.data.frame(data[,ignore.date]))
	
	if (!is.null(extract)) {
		out <- lapply(X=out,FUN=function(x,extract) {x[which(x$spell_state %in% extract),]},extract=extract)
		if (only.inner==TRUE) {
			
			out <- lapply(X=out,FUN=function(x,month) {x[which(lubridate::month(x$start_date) %in% month),]},month=month)
			out <- lapply(X=out,FUN=function(x,month) {x[which(lubridate::month(x$end_date) %in% month),]},month=month)
			
		} else {
			
			out <- lapply(X=out,FUN=function(x,month) {x[which(x$month %in% month),]},month=month)
		}
	}
	
	## FARE UN OPZIONE PER IL MELTING ..... 
	
	if (melting.df==TRUE) {
		
		warning("Melting into a df: this option require that extract argument be 'wet' or 'dry' !!")
		
		out_df <- as.data.frame(array(NA,c(nrdata,length(out))))
		names(out_df) <- names(out)
		out_df <- adddate(out_df,origin=origin)
		index <- sprintf("%04d-%02d-%02d",out_df$year,out_df$month,out_df$day)
		out_df$Date <- as.Date(index)
		
		for (i in names(out)) {
			
			name <- i
			
			index_out <- sprintf("%04d-%02d-%02d",out[[i]]$year,out[[i]]$month,out[[i]]$day)
			index_out <- as.Date(index_out)
			out_df[,i] <- 0*NA
			
		
		
			out_df[out_df$Date %in% index_out,i] <- out[[i]]$spell_length
			
		
		}
		
		
		out <- out_df
		
		
	}
	
	
	return(out)
}