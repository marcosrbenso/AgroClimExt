library(tidyverse)
library(zoo)
library(SPEI)
library(Evapotranspiration)
library(geobr)
library(sf)



##### Functions #######

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

##### Prec #####

lapply(data$data,function(x){

  x.ts <- zoo(x$prec,x$date)
  x.ts.m <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), sum)


}) -> prec.index


##### SPI #####

sow.month = 10
harvest.month = 1

lapply(prec.index,function(x){

  index <- zoo(spi(ts(as.vector(x),freq=12,start=c(1980,1),end=c(2020,7)),3)$fitted,
               date(x))

}) -> spi.index


##### SPEI #####

map <- read_municipality()

map <- map %>% subset(name_muni %in% data$name_mn & abbrev_state == "PR")

lat <- st_coordinates(st_centroid(map))[,1]

data("constants")

index(data$data[data$data == data$data[[1]]])

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

}) -> spei.index


##### FD, Number of frost days #######


lapply(data$data[1],function(x){
  x.ts <- zoo(x$tmin,x$date)
  rollapply(x.ts,90,function(x){length(x[x<1])},align = "right")
})

##### SU, Number of summer days #####


lapply(data$data[1],function(x){
  x.ts <- zoo(x$tmax,x$date)
  rollapply(x.ts,90,function(x){length(x[x>25])},align = "right")
})


###### ID, Number of icing days ######

lapply(data$data[1],function(x){
  x.ts <- zoo(x$tmin,x$date)
  rollapply(x.ts,90,function(x){length(x[x<0])},align = "right")
})


###### TR, Number of tropical nights ######

lapply(data$data[1],function(x){
  x.ts <- zoo(x$tmin,x$date)
  rollapply(x.ts,90,function(x){length(x[x>20])},align = "right")
})


##### TXx, Monthly maximum value of daily maximum temperature #####

lapply(data$data,function(x){

  x.ts <- zoo(x$tmax,x$date)
  x.ts.m <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)

})

###### TNx, Monthly maximum value of daily minimum temperature #######


lapply(data$data,function(x){

  x.ts <- zoo(x$tmin,x$date)
  x.ts.m <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)

})


###### TXn, Monthly minimum value of daily maximum temperature ##########


lapply(data$data,function(x){

  x.ts <- zoo(x$tmax,x$date)
  x.ts.m <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), min)

})


###### TNn, Monthly minimum value of daily minimum temperature: #########

lapply(data$data,function(x){

  x.ts <- zoo(x$tmin,x$date)
  x.ts.m <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), min)

})

####### TX10p, Percentage of days when TX < 10th percentile #######

library(boot)

# Determine the quantiles

quantile_10 <- function(d, i){
  d2 <- d[i,]
  return(quantile(d2,0.1))
}

quantile_90 <- function(d, i){
  d2 <- d[i,]
  return(quantile(d2,0.9))
}

lapply(data$data,function(x){

  x.ts <- zoo(x$tmax,x$date)
  x.ts <- rollmean(x.ts,5,align = "center")
  boot(x.ts, quantile_10, R=5000)$t0

}) -> TX10p.quantile


