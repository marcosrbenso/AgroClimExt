#  Crop yield data warping and preparing

# Three datasets are use for Soybean and Maize second cycle in Brazil
## Deral - Department of Rural Economics of Paraná State-Brazil
## IBGE - Brazilian Institute of Geography and Satistics
## GDHY - Global Dataset on Historical Yields

# Datasets are agregated at municipality level
# The objective of this script is to prepare data for Machine Learning Models

# Marcos Benso, Ago 2024


rm(list=ls())
gc()

#======================================================================================================
# Load packages

library(tidyverse)
library(geobr)
library(sf)
library(sp)
library(textclean)

#======================================================================================================
# Set working directory with data

setwd("C:\\Users\\marco\\Downloads\\agroclimx\\AgroClimExt")

#======================================================================================================
# Load data

data.maize.ibge <- read.csv("Database/Crop/Processed_Data/ibge_maize.csv")
data.soy.ibge <- read.csv("Database/Crop/Processed_Data/ibge_soy.csv")

data.maize.deral <- read.csv("Database/Crop/Processed_Data/deral_maize.csv")
data.soy.deral <- read.csv("Database/Crop/Processed_Data/deral_soybean.csv")

data.maize.gdhy <- read.csv("Database/Crop/Processed_Data/gdhy.maize.csv")
data.soy.gdhy <- read.csv("Database/Crop/Processed_Data/gdhy.soy.csv")

#======================================================================================================
# Organize data.frame

data.maize.ibge <- data.maize.ibge[,c('State','City','year','yield')]
colnames(data.maize.ibge) <- c("UF","City","Year","Yield")
data.maize.ibge$dataset = "IBGE"

data.soy.ibge <- data.soy.ibge[,c('State','City','year','yield')]
colnames(data.soy.ibge) <- c("UF","City","Year","Yield")
data.soy.ibge$dataset = "IBGE"

data.maize.deral <- data.maize.deral[,c('Município','harvest','yield')]
colnames(data.maize.deral) <- c("City","Year","Yield")
data.maize.deral$UF <- "PR"
data.maize.deral$dataset = "DERAL"

data.soy.deral<- data.soy.deral[,c('Município','harvest','yield')]
colnames(data.soy.deral) <-  c("City","Year","Yield")
data.soy.deral$UF <- "PR"
data.soy.deral$dataset = "DERAL"

data.maize.gdhy <- data.maize.gdhy[,c('state','city','year','yield')]
colnames(data.maize.gdhy) <- c("UF","City","Year","Yield")
data.maize.gdhy$dataset = "GDHY"

data.soy.gdhy <- data.soy.gdhy[,c('state','city','year','yield')]
colnames(data.soy.gdhy) <- c("UF","City","Year","Yield")
data.soy.gdhy$dataset = "GDHY"

#======================================================================================================
# Merge function

br <- read_municipality() ## Load sf data for brazilian municipalities

merge_function <- function(data,city = "City",state = "UF"){
  data[,"City"] <- strip(data[,"City"], char.keep = NULL, digit.remove = TRUE,
                                     apostrophe.remove = TRUE, lower.case = TRUE) %>%
    iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')
  merge(data,
        br %>%
          mutate(name_muni = strip(name_muni, char.keep = NULL, digit.remove = TRUE,
                                   apostrophe.remove = TRUE, lower.case = TRUE)) %>%
          mutate(name_muni = iconv(name_muni, from = 'UTF-8', to = 'ASCII//TRANSLIT')),
        by.x=c(city,state),
        by.y=c('name_muni','abbrev_state')) -> new_data
  return(new_data)
}


#======================================================================================================
# Build a crop dataset

soybean <- rbind(data.soy.deral,
                 data.soy.gdhy,
                 data.soy.ibge)


maize <- rbind(data.maize.deral,
               data.maize.gdhy,
               data.maize.ibge)


soybean <- merge_function(soybean)
maize <- merge_function(maize)

#======================================================================================================
# Remove Trend

year_min <- 10 # minimum number of years to include municipality in the dataset

# Test for trend

soybean <- soybean %>%
  group_by(City,dataset) %>%
  mutate(trend = ifelse(Kendall(year, yield)$sl[1] < 0.05,"Trend","No Trend"),
         tau = Kendall(year, yield)$tau[1])

maize <- maize %>%
  group_by(City,dataset) %>%
  mutate(trend = ifelse(Kendall(year, yield)$sl[1] < 0.05,"Trend","No Trend"),
         tau = Kendall(year, yield)$tau[1])

# Detrend series

crop_yield_detrend <- function(yield,year){
  model <- loess(yield~year, se = TRUE)
  yield-predict(model,data.frame(yield=yield,year=year))
}

soybean <- soybean %>%
  group_by(City,dataset) %>%
  filter(is.na(yield)==F) %>%
  group_by(code_muni) %>%
  filter(length(code_muni) > year_min) %>%
  mutate(yield_detrended = crop_yield_detrend(yield,year))

maize <- maize %>%
  group_by(City,dataset) %>%
  filter(is.na(yield)==F) %>%
  group_by(code_muni) %>%
  filter(length(code_muni) > year_min) %>%
  mutate(yield_detrended = crop_yield_detrend(yield,year))



#======================================================================================================
# Test for Heteroscedasticity

soybean <- soybean %>%
  group_by(City,dataset) %>%
  mutate(Hetero = bptest(yield~year)$p.value,
         Hetero = ifelse(Hetero < 0.05,"Heteroskedastic","Homoscedastic"))

maize <- maize %>%
  group_by(City,dataset) %>%
  mutate(Hetero = bptest(yield~year)$p.value,
         Hetero = ifelse(Hetero < 0.05,"Heteroskedastic","Homoscedastic"))


# Remove Heteroscedasticity
crop_yield_heteroscedasticity <- function(yield,year,hetero){

  model <- loess(yield~year)
  y_max <- max(year)-1
  if(hetero == "Homoscedastic"){

    (yield-predict(model,data.frame(yield=yield,year=year)))+
      predict(model,data.frame(year=y_max))

  }else{

    (1+(yield-predict(model,data.frame(yield=yield,year=year)))/
       predict(model,data.frame(yield=yield,year=year)))*
      predict(model,data.frame(year=y_max))
  }
}

soybean <- soybean %>%
  group_by(City,dataset) %>%
  mutate(yield_corrected = crop_yield_anomaly(yield,year,Hetero))

maize <- maize %>%
  group_by(City,dataset) %>%
  mutate(yield_corrected = crop_yield_anomaly(yield,year,Hetero))

#======================================================================================================
# Remove outliers and calculate spatial blocks


#======================================================================================================
# Load climate indices

indices_soy <- fread("soybean_indices.csv")
indices_maize <- fread("maize_indices.csv")


#======================================================================================================
# Build final dataset with crop yield and climate indices


dataset_soybean <- merge(soybean,
                 indices_soy,
                 by.x = c("code_muni","Year"),
                 by.y = c("name_mn","harvesting"))%>% na.omit()

dataset_maize <- merge(maize,
                 indices_soy,
                 by.x = c("code_muni","Year"),
                 by.y = c("name_mn","harvesting"))%>% na.omit()





