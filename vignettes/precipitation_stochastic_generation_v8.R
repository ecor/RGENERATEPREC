## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(echo = TRUE)


## ----eval=TRUE,echo=TRUE,results='hide',warning=FALSE,message=FALSE-----------

library(RGENERATEPREC)
library(lubridate)

## ----eval=TRUE----------------------------------------------------------------

data(trentino)


## ----echo=TRUE,return=TRUE,warning=FALSE,results='markup',message=FALSE,fig.width=7----

library(sf)
library(mapview)

trentino_stations <- data.frame(x=STATION_LATLON[,1],y=STATION_LATLON[,2],name=STATION_NAMES)
######
trentino_stations$geometry <- trentino_stations[,c("x","y")] %>% t() %>% as.data.frame() %>% as.list() %>% lapply(st_point) %>% st_sfc()
trentino_stations <- st_sf(trentino_stations,geometry=trentino_stations$geometry,crs=4326)
mapview(trentino_stations,col.regions="blue")


## ----eval=TRUE,results='hide',message=FALSE-----------------------------------

str(TEMPERATURE_MAX)
str(TEMPERATURE_MIN)
str(PRECIPITATION)
  

## ----eval=TRUE----------------------------------------------------------------
year_min <- 1961
year_max <- 1990

origin <- paste(year_min,1,1,sep="-")
period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

## ----eval=TRUE----------------------------------------------------------------
prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]

## ----eval=TRUE----------------------------------------------------------------
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	acc <- TRUE
	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
	
}



## ----eval=TRUE----------------------------------------------------------------
names(accepted)


## ----eval=TRUE----------------------------------------------------------------

prec_mes <- prec_mes[,accepted]
head(prec_mes)

## ----eval=TRUE----------------------------------------------------------------
Tx_mes <- Tx_mes[,accepted]
head(Tx_mes)

## ----eval=TRUE----------------------------------------------------------------
Tn_mes <- Tn_mes[,accepted]
head(Tn_mes)

## ----eval=TRUE----------------------------------------------------------------
valmin <- 1.0
prec_occurence_mes <- prec_mes
station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
prec_occurence_mes[,station] <- prec_mes[,station]>=valmin
head(prec_occurence_mes)

## ----eval=TRUE----------------------------------------------------------------
it1 <- "T0083" #station[2]
it1

## ----eva=TRUE-----------------------------------------------------------------
exogen <- Tx_mes[,it1]-Tn_mes[,it1]
months <- factor(prec_mes$month)
model1 <- PrecipitationOccurrenceModel(x=prec_mes[,it1],exogen=exogen,monthly.factor=months)

## ----eval=TRUE----------------------------------------------------------------
class(model1)

## ----eval=TRUE----------------------------------------------------------------
class(unclass(model1))
names(model1)

## ----eval=TRUE----------------------------------------------------------------
str(model1$predictor)

## ----eval=TRUE----------------------------------------------------------------
summary(model1$glm)

## ----eval=TRUE----------------------------------------------------------------
model1$p

## ----eval=TRUE----------------------------------------------------------------
model1$valmin

## ----eval=TRUE----------------------------------------------------------------
probs <- predict(model1$glm,type="response")

## ----eval=TRUE----------------------------------------------------------------

row_test <- 2000:2007
newdata <- model1$predictor[row_test,]

## ----eval=TRUE----------------------------------------------------------------

head(newdata)
newdata$probs2 <- predict(model1,newdata=newdata)

head(newdata)

## ----eval=TRUE----------------------------------------------------------------
exogen <- exogen
months <- factor(prec_mes$month)
set.seed(1235)
prec_gen1_occ <- generate(model1,exogen=exogen,monthly.factor=months,n=length(months))


## ----eval=TRUE----------------------------------------------------------------

model1_amount <- PrecipitationAmountModel(prec_mes,station=it1,origin=origin)

## ----eval=TRUE----------------------------------------------------------------
class(model1_amount)

## ----eval=TRUE----------------------------------------------------------------
names(model1_amount)

## ----eval=TRUE----------------------------------------------------------------

summary(model1_amount[[it1]])


## ----eval=TRUE----------------------------------------------------------------

model1_amount$station
model1_amount$sample
model1_amount$origin
model1_amount$valmin


## ----eval=TRUE----------------------------------------------------------------

str(model1_amount$x)


## ----eval=TRUE----------------------------------------------------------------

prec_gen1 <- generate(model1_amount,newdata=prec_gen1_occ)
str(prec_gen1)

## ----eval=TRUE,fig.width=12---------------------------------------------------

library(ggplot2)
library(lubridate)
library(reshape2)
df <- data.frame(obs=prec_mes[,it1],gen=prec_gen1[,it1])
df$date <- as.Date(origin)+days(1:nrow(df))-1
df$month <- factor(month(df$date))
df$season <- "none"
df$season[df$month %in% c(12,2,1)] <- "`1.DJF"
df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
df$season[df$month %in% c(9,10,11)] <-  "4.SON"
qqplot_ <- function(df) {
  df <- as.list(df)
  o <- qqplot(df[[1]],df[[2]],plot.it=FALSE)
  names(o) <- names(df)[1:2]
  o <- as.data.frame(o)
  return(o)
  
}

qqdf <- split(df,f=df$season) %>% lapply(FUN=qqplot_) %>% melt(id=names(df)[1:2])
names(qqdf)[names(qqdf)=="L1"] <- "season"
g <- ggplot(data=qqdf)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(. ~ season)+xlab("observed")+ylab("generated")
show(g)


## ----eval=TRUE,echo=TRUE------------------------------------------------------
dw <- list()
dw$obs <- dw.spell(prec_mes[,it1],origin=origin)[[1]]
dw$obs$variable <- "obs"
dw$gen <- dw.spell(prec_gen1[,it1],origin=origin)[[1]]
dw$gen$variable <- "gen"
dw <- do.call(what=rbind,args=dw)
dw$season <- "none"
dw$season[dw$month %in% c(12,2,1)] <- "`1.DJF"
dw$season[dw$month %in% c(3,4,5)] <-  "2.MAM"
dw$season[dw$month %in% c(6,7,8)] <-  "3.JJA"
dw$season[dw$month %in% c(9,10,11)] <-  "4.SON"  

qqdw <- split(dw,f=paste(dw$spell_state,dw$season,sep="_")) %>% lapply(FUN=function(x){list(obs=x$spell_length[x$variable=="obs"],gen=x$spell_length[x$variable=="gen"])}) %>% lapply(FUN=qqplot_) %>% melt(id=c("obs","gen"))
qqdw_add <- str_split(qqdw$L1,pattern="_") %>% do.call(what=rbind) %>% as.data.frame()
names(qqdw_add) <- c("state","season")
qqdw <- cbind(qqdw,qqdw_add)



## ----eval=TRUE,fig.width=12---------------------------------------------------
qqdry <- qqdw[qqdw$state=="dry",]

## ggplot plot
ggdry <- ggplot(data=qqdry)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(. ~ season)+xlab("observed")+ylab("generated")
show(ggdry)


## ----eval=TRUE,fig.width=12---------------------------------------------------
qqwet <- qqdw[qqdw$state=="wet",]

## ggplot plot
ggwet <- ggplot(data=qqwet)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(. ~ season)+xlab("observed")+ylab("generated")
show(ggwet)


## ----eval=TRUE,message=FALSE--------------------------------------------------
they <- c("T0083","T0090") ##station
exogen <- Tx_mes[,they]-Tn_mes[,they]
months <- factor(prec_mes$month)
model <- PrecipitationOccurrenceMultiSiteModel(x=prec_mes[,they],exogen=exogen,origin=origin)




## ----eval=TRUE----------------------------------------------------------------
names(model)

## ----eval=TRUE----------------------------------------------------------------
class(model[[1]])

## ----eval=TRUE----------------------------------------------------------------
class(model$ccgamma)

## ----eval=TRUE----------------------------------------------------------------
class(model$K)

## ----eval=TRUE----------------------------------------------------------------
class(model$type)

## ----eval=TRUE----------------------------------------------------------------
class(model$station)

## ----eval=TRUE----------------------------------------------------------------
class(model$p)

## ----eval=TRUE----------------------------------------------------------------
prec_gen_occ <- generate(model,exogen=exogen,monthly.factor=months,n=length(months))
str(prec_gen_occ)

## ----eval=TRUE----------------------------------------------------------------

model_amount <- PrecipitationAmountModel(prec_mes,station=they,origin=origin)
names(model_amount)


## ----eval=TRUE----------------------------------------------------------------
class(model_amount)

## ----eval=TRUE----------------------------------------------------------------
names(model_amount)

## ----eval=TRUE----------------------------------------------------------------
for (its in model_amount$station) summary(model_amount[[its]])

## ----eval=TRUE----------------------------------------------------------------

model_amount$station
model_amount$sample
model_amount$origin
model_amount$valmin


## ----eval=TRUE----------------------------------------------------------------

prec_gen <- generate(model_amount,newdata=prec_gen_occ)

names(prec_gen)

## ----eval=TRUE,fig.width=12---------------------------------------------------

library(ggplot2)
library(lubridate)
library(reshape2)

str(prec_mes)
str(prec_gen)

df <- list(obs=prec_mes[names(prec_gen)],gen=prec_gen)
for ( i in 1:length(df)) {
  df[[i]]$date <- as.Date(origin)+days(1:nrow(df[[i]]))-1
}


df <- melt(df,id="date")
names(df)[names(df)=="variable"] <- "station"
names(df)[names(df)=="L1"] <- "variable"
df$month <- factor(month(df$date))
df$season <- "none"
df$season[df$month %in% c(12,2,1)] <- "1.DJF"
df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
df$season[df$month %in% c(9,10,11)] <-  "4.SON"

qqdf <- split(df,f=paste(df$station,df$season,sep="_")) %>% 
lapply(FUN=function(x){list(obs=x$value[x$variable=="obs"],gen=x$value[x$variable=="gen"])}) %>% lapply(FUN=qqplot_) %>% melt(id=c("obs","gen"))
qqdf_add <- str_split(qqdf$L1,pattern="_") %>% do.call(what=rbind) %>% as.data.frame()
names(qqdf_add) <- c("station","season")
qqdf <- cbind(qqdf,qqdf_add)

## ggplot plot
ggdf <- ggplot(data=qqdf)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(station ~ season)+xlab("observed")+ylab("generated")
show(ggdry)
show(ggdf)


## ----eval=TRUE,echo=TRUE------------------------------------------------------
dw <- list()
dw$obs <- dw.spell(prec_mes[,they],origin=origin)
nn <- names(dw$obs[[1]])
dw$obs <- melt(dw$obs,id=nn)
dw$obs$variable <- "obs"
dw$gen <- dw.spell(prec_gen[,they],origin=origin) %>% melt(id=nn)
dw$gen$variable <- "gen"

dw <- do.call(what=rbind,args=dw)
names(dw)[names(dw)=="L1"] <- "station"

dw$season <- "none"
dw$season[dw$month %in% c(12,2,1)] <- "`1.DJF"
dw$season[dw$month %in% c(3,4,5)] <-  "2.MAM"
dw$season[dw$month %in% c(6,7,8)] <-  "3.JJA"
dw$season[dw$month %in% c(9,10,11)] <-  "4.SON"  

qqdw <- split(dw,f=paste(dw$spell_state,dw$station,dw$season,sep="_")) %>% lapply(FUN=function(x){list(obs=x$spell_length[x$variable=="obs"],gen=x$spell_length[x$variable=="gen"])}) %>% lapply(FUN=qqplot_) %>% melt(id=c("obs","gen"))

qqdw_add <- str_split(qqdw$L1,pattern="_") %>% do.call(what=rbind) %>% as.data.frame()
names(qqdw_add) <- c("state","station","season")
qqdw <- cbind(qqdw,qqdw_add)



## ----eval=TRUE,fig.width=12---------------------------------------------------
qqdry <- qqdw[qqdw$state=="dry",]

## ggplot plot
ggdry <- ggplot(data=qqdry)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(station  ~ season)+xlab("observed")+ylab("generated")
show(ggdry)


## ----eval=TRUE,fig.width=12---------------------------------------------------
qqwet <- qqdw[qqdw$state=="wet",]

## ggplot plot
ggwet <- ggplot(data=qqwet)+geom_point(aes(x=obs,y=gen))+theme_bw()+geom_abline()+facet_grid(station  ~ season)+xlab("observed")+ylab("generated")
show(ggwet)


## ----eval=TRUE,warning=FALSE,output=FALSE-------------------------------------
      months <- unique(prec_mes$month)
      ks_test <- list()
      for (m in months) {
        
        ks_test[[m]] <- ks.test(prec_mes[prec_mes$month==m,it1],prec_gen[prec_mes$month==m,it1])
      }
      ks_test


## ----eval=TRUE,echo=TRUE,message=FALSE,fig.width=12---------------------------
str(prec_mes[,they])
str(prec_gen[,they])
cc_mes <- CCGamma(prec_mes[,they],sample = "monthly",origin = origin)
cc_gen <- CCGamma(prec_gen[,they],sample = "monthly",origin = origin)

## ----eval=TRUE,echo=TRUE,results="hide",fig.width=12--------------------------
str(cc_mes)
str(cc_gen)

## ----eval=TRUE,echo=FALSE,fig.width=12----------------------------------------
cc_mes %>% lapply(function(x){x$nooccurrence_correlation})

## ----eval=TRUE,echo=FALSE,fig.width=12----------------------------------------
cc_gen %>% lapply(function(x){x$nooccurrence_correlation})

## ----eval=TRUE----------------------------------------------------------------
df <- data.frame(obs=prec_mes[,it1],gen=prec_gen1[,it1])
df$date <- as.Date(origin)+days(1:nrow(df))-1
df$month <- factor(month(df$date))
df$season <- "none"
df$season[df$month %in% c(12,2,1)] <- "1.DJF"
df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
df$season[df$month %in% c(9,10,11)] <-  "4.SON"

dfp <- df[df$obs>valmin,] 

## ----eval=TRUE,fig.width=7----------------------------------------------------

color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]

gdens <- ggplot(data=dfp)+geom_density(aes(x=obs,color=month,group=month),trim=TRUE)+facet_grid(season ~ .)+theme_bw()
gdens <- gdens+scale_color_manual(values = color_month)

show(gdens)


## ----eval=TRUE----------------------------------------------------------------

library(lmom)

prec_val_m <- dfp$obs %>% split(dfp$month) 
lmoms <- prec_val_m %>% lapply(FUN=samlmu) 

params <- lmoms %>% lapply(FUN=pelln3,bound=valmin) 
kstest <-params %>% mapply(FUN=ks.test,x=prec_val_m,y="cdfln3",SIMPLIFY=FALSE) 

kstest


## ----eval=TRUE,fig.width=7----------------------------------------------------

modify_lmoments <- function(x){x[2] <- x[2]*1.3; return(x)}
paraml <- function(x,valmin) {
                  ip <- which(x>=valmin)
                  x <- x[ip] 
                  out <- list()
                  lmomx <- samlmu(x)
                  out$obs <- pelln3(lmom=lmomx,bound=valmin)
                  lmomx_m <- modify_lmoments(lmomx)
                  out$mod  <- pelln3(lmom=lmomx_m,bound=valmin)
                  return(out)
}

modify_distribution <- function(x,paraml,valmin){
                        
                        out <- x
                        ip <- which(x>=valmin)
                        out[ip] <- x[ip] %>% cdfln3(para=paraml$obs) %>% qualn3(para=paraml$mod)
                        return(out)
    
}


para_monthly <- df$obs %>% split(df$month) %>% lapply(FUN=paraml,valmin=valmin)

df$obs_mod <- df$obs
for (mo in unique(df$month))  {
  im <- which(df$month==mo)
  
  df$obs_mod[im] <- modify_distribution(x=df$obs[im],paraml=para_monthly[[mo]],valmin=valmin)
}
color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
gg <- ggplot()+geom_line(data=df,mapping=aes(x=obs,y=obs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
gg <- gg+scale_color_manual(values = color_month)
show(gg)

## ----eval=TRUE----------------------------------------------------------------
prec_mod <- as.data.frame(df$obs_mod) 
names(prec_mod) <- it1
model1_amount_mod <- PrecipitationAmountModel(prec_mod,station=it1,origin=origin)
prec_gen1_mod <- generate(model1_amount_mod,newdata=prec_gen1_occ)
str(prec_gen1_mod)


## ----eval=TRUE,fig.width=12---------------------------------------------------
df$gen_mod <- prec_gen1_mod[,it1]

qqplot__ <- function(df,i=1) {
  df <- as.list(df)
  o <- list()
  nn <- names(df)
  jv <- (1:length(df))[-i]
  for (j in jv) {
    ot <- qqplot(df[[i]],df[[j]],plot.it=FALSE)
    ot <- as.data.frame(ot)
    names(ot) <- names(df)[c(i,j)]
    o[[j]] <- ot
  }
  ref <- o[[jv[1]]][,1]
  for (j in jv[-1]) {
    ref2 <-  o[[j]][,1]
    cond <- all(ref==ref2)
    if (cond==FALSE) stop("qqplot__ error")
    
    
  }
  o <- do.call(args=o[jv],what=cbind)
  o <- o[,nn] %>% melt(id=1)
  
  
  return(o)
}

nnn <- c("obs_mod","gen_mod","obs")

qqdf <- split(df[,nnn],f=df$season) %>% lapply(FUN=qqplot__) %>% melt(id=1:3)
names(qqdf)[names(qqdf)=="L1"] <- "season"
g <- ggplot(data=qqdf)+geom_point(aes(x=obs_mod,y=value,group=variable,color=variable))+theme_bw()+geom_abline()+facet_grid(. ~ season)+xlab("modified secenario through observated values [mm]")+ylab("generated / observated [mm]")
show(g)


