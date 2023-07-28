library(tidyverse)
library(zoo)
library(SPEI)
library(Evapotranspiration)
library(geobr)
library(sf)

##### Functions #######

CDD <- function(x){
  x <- ifelse(x>1,1,0)
  foo <- rle(x)
  res <- data.frame(lengths=foo$lengths,values=foo$values)
  res %>% subset(values==0) %>% summarise(CDD = max(lengths)) %>% as.numeric()
}

CWD <- function(x){
  x <- ifelse(x>1,1,0)
  foo <- rle(x)
  res <- data.frame(lengths=foo$lengths,values=foo$values)
  res %>% subset(values!=0) %>% summarise(CDD = max(lengths)) %>% as.numeric()
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



##### SPI #####

i <- 0
lapply(prec.index,function(x){

  i <<- i+1
  print(c(map$name_muni[i],i))

  index <- zoo(spi(ts(as.vector(x),freq=12,start=c(1980,1),end=c(2020,7)),3)$fitted,
               date(x))

  path <- paste0(path,"spi_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> spi.index



###### Rx1day, Monthly maximum 1-day precipitation ######
i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)
  path <- paste0(path,"rx1day_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> rx1day.index

###### Rx5day, Monthly maximum consecutive 5-day precipitation ########

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  x.ts <- rollsum(x.ts,k=5,align = "right")
  index <- aggregate(x.ts, as.Date(as.yearmon(time(x.ts))), max)
  path <- paste0(path,"rx5day_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)

}) -> rx5day.index

###### SDII Simple pricipitation intensity index ########

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  index <- rollapply(x.ts,90,function(x){sum(x[x>1])/length(x[x>1])},align = "right")
  path <- paste0(path,"sdii_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> sdii.index


####### R10mm Annual count of days when PRCP≥ 10mm #######

i < - 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  index <- rollapply(x.ts,90,function(x){length(x[x>10])},align = "right")
  path <- paste0(path,"r10mm_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> r10mm.index


####### R20mm Annual count of days when PRCP≥ 20mm #########

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  rollapply(x.ts,90,function(x){length(x[x>20])},align = "right")
  path <- paste0(path,"r20mm_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> r20mm.index


##### CDD. Maximum length of dry spell, maximum number of consecutive ########

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  index <- rollapply(x.ts,90,CDD,align = "right")
  path <- paste0(path,"cdd_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> cdd.index

###### CWD. Maximum length of wet spell, maximum number of consecutive ########

i <- 0
lapply(data$data,function(x){
  i <<- i+1
  x.ts <- zoo(x$prec,x$date)
  index <- rollapply(x.ts,90,CWD,align = "right")
  path <- paste0(path,"cwd_",map$name_muni[i],".csv")
  write.csv(index,path)

  return(index)
}) -> cwd.index
