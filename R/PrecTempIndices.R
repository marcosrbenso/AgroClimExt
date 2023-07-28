library(tidyverse)
library(zoo)
library(SPEI)
library(Evapotranspiration)
library(geobr)
library(sf)


precesseddata <- function(data){
  Date.daily <- data$date
  Date.monthly <- as.yearmon(unique(paste(year(data$date),month(data$date))), "%Y %m")
  J <- zoo(x = as.integer(format((data$date), "%j")), data$date)
  i <- month(Date.monthly)
  Date.daily = Date.daily
  Date.monthly = Date.monthly
  Ndays = zoo(days_in_month(Date.monthly),Date.monthly)
  Tmax = zoo(data$tmax,Date.daily)
  Tmin = zoo(data$tmin,Date.daily)
  Tmed = zoo((Tmax+Tmin)/2)
  Tmed.monthly = aggregate(Tmed,
                           as.yearmon(paste(year(Date.daily),month(Date.daily)), "%Y %m"), mean)
  Precip = zoo(data$prec,Date.daily)
  P.monthly = aggregate(Precip,
                        as.yearmon(paste(year(Date.daily),month(Date.daily)), "%Y %m"), sum)
  res <- list(
    Date.daily = Date.daily,
    Date.monthly = Date.monthly,
    J = J,
    i = i,
    Ndays = Ndays,
    Tmax = Tmax,
    Tmin = Tmin,
    Tmed = Tmed,
    Tmed.monthly = Tmed.monthly,
    Tdew = NULL,
    RHmax = NULL,
    RHmin = NULL,
    u2 = NULL,
    uz = NULL,
    Rs = NULL,
    n = NULL,
    Cd = NULL,
    Precip = Precip,
    P.monthly = P.monthly,
    Epan = NULL,
    va = NULL,
    vs = NULL
  )

  return(
    res
  )
}



##### Import data ########

dataset <- read.csv('era5.csv') # read data in form of data.frame

dataset$date <- as.Date(dataset$date)

##### Calculate Indices #####

dataset %>% group_by(name_mn) %>% nest() -> data # nested dataset

##### Maps ######

map <- read_municipality()

map <- map %>% subset(name_muni %in% data$name_mn & abbrev_state == "PR")

lat <- st_coordinates(st_centroid(map))[,1]

path <- "/home/marcosrb/AgroClimExt/Output/Indices_raw/"

##### SPEI #####


data("constants")

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  print(i)
  data <- precesseddata(x)
  constants$lat <- lat[i]
  eto <- ET.HargreavesSamani(data, constants, ts="daily", message="yes",
                             AdditionalStats="yes", save.csv="no")$ET.Monthly
  prec <- data$P.monthly

  index <- zoo(spei(ts(as.vector(prec)-as.vector(eto),freq=12,start=c(1980,1),end=c(2020,7)),3)$fitted,
               date(prec))

  path <- paste0(path,"spei_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)


}) -> spei.index


##### FD, Number of frost days #######

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$tmin,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x<1])},align = "right")
  path <- paste0(path,"fd_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> fd.index

##### SU, Number of summer days #####

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$tmax,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x>25])},align = "right")
  path <- paste0(path,"spi_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> su.index


###### ID, Number of icing days ######

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$tmin,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x<0])},align = "right")
  path <- paste0(path,"id_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> id.index


###### TR, Number of tropical nights ######
i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$tmin,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x>20])},align = "right")
  path <- paste0(path,"tr_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> tr.index


##### TXx, Monthly maximum value of daily maximum temperature #####

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$tmax,x$date)
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)
  path <- paste0(path,"txx_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> txx.index

###### TNx, Monthly maximum value of daily minimum temperature #######

i <- 0
lapply(data$data,function(x){
  i <<- i+1

  x.ts <- zoo(x$tmin,x$date)
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)
  path <- paste0(path,"tnx_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> tnx.index


###### TXn, Monthly minimum value of daily maximum temperature ##########

i <- 0
lapply(data$data,function(x){
  i <<- i+1

  x.ts <- zoo(x$tmax,x$date)
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), min)
  path <- paste0(path,"txn_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> txn.index


###### TNn, Monthly minimum value of daily minimum temperature: #########

i <- 0
lapply(data$data,function(x){
  i <<- i+1

  x.ts <- zoo(x$tmin,x$date)
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), min)
  path <- paste0(path,"txn_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> tnn.index



library(boot)

# Determine the percetiles

quantile_10 <- function(d, i){
  d2 <- d[i,]
  return(quantile(d2,0.1))
}

quantile_90 <- function(d, i){
  d2 <- d[i,]
  return(quantile(d2,0.9))
}


####### TX10p, Percentage of days when TX < 10th percentile #######


lapply(data$data,function(x){

  x.ts <- zoo(x$tmax,x$date)
  x.ts <- rollmean(x.ts,5,align = "center")
  boot(x.ts, quantile_10, R=500)$t0

}) -> x10p.quantile

i <- 0
mapply(function(x,y){

  i <<- i+1
  x.ts <- zoo(x$tmax,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x<y])},align = "right")
  path <- paste0(path,"tx10p_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}, data$data,x10p.quantile) -> TX10p.index



###### TN10p, Percentage of days when TN < 10th percentile #####

lapply(data$data,function(x){

  x.ts <- zoo(x$tmin,x$date)
  x.ts <- rollmean(x.ts,5,align = "center")
  boot(x.ts, quantile_10, R=500)$t0

}) -> n10p.quantile

i <- 0
mapply(function(x,y){
  i <<- i+1
  x.ts <- zoo(x$tmin,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x<y])},align = "right")
  path <- paste0(path,"tx10p_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}, data$data,n10p.quantile) -> TN10p.index

#### TX90p, Percentage of days when TX < 10th percentil


lapply(data$data,function(x){


  x.ts <- zoo(x$tmax,x$date)
  x.ts <- rollmean(x.ts,5,align = "center")
  boot(x.ts, quantile_90, R=500)$t0

}) -> x90p.quantile

i <- 0
mapply(function(x,y){
  i <<- i+1
  x.ts <- zoo(x$tmax,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x>y])},align = "right")
  path <- paste0(path,"tx10p_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}, data$data,x90p.quantile) -> TX90p.index

##### TN90p, Percentage of days when TN > 90th percentile #######

lapply(data$data,function(x){

  x.ts <- zoo(x$tmax,x$date)
  x.ts <- rollmean(x.ts,5,align = "center")
  boot(x.ts, quantile_90, R=500)$t0

}) -> n90p.quantile

i <- 0
mapply(function(x,y){
  i <<- i+1
  x.ts <- zoo(x$tmax,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x>y])},align = "right")
  path <- paste0(path,"tx10p_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}, data$data,n90p.quantile) -> TN90p.index

plot(TN90p.index[,4],type='l')
