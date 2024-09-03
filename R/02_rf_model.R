#  Random forest model for crop yield losses predictions

# The objective of this script is to run RF model
# for evaluating the impact of climate extremes on crop yields

# Marcos Benso, Ago 2024


#======================================================================================================
# Load packages

library(doParallel)
library(varImp)
library(sf)
library(caret)
library(data.table)
library(tidyverse)

#======================================================================================================
# Output file

output <- "C:\\Projetos\\ClimateImpactML\\Data\\Output\\Step1\\"

set.seed(123)

#======================================================================================================
# Pre-processing function

filter <-function(dataset_sub){
  if("trend" %in% colnames(dataset_sub)){
    dataset_sub <- dataset_sub %>% ungroup() %>%
      select(-c(Year,Hetero,tau,trend,
                code_muni,Yield_detrended,
                City,UF,Yield,code_state,lat,
                clay,silt,sand))
  }
  else{
    dataset_sub <- dataset_sub %>% ungroup() %>%
      select(-c(Year,Hetero,tau,
                code_muni,Yield_detrended,
                City,UF,Yield,code_state,lat,
                clay,silt,sand))
  }
  #------------------------------------------------------
  # Remove near Zero variance features
  nzv <- nearZeroVar(dataset_sub)

  filteredDescr <- dataset_sub[, -nzv]

  #------------------------------------------------------
  # Remove highly correlated variables
  descrCor <-  cor(filteredDescr)

  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .99)

  highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)

  filteredDescr <- filteredDescr[,-highlyCorDescr]
  return(colnames(filteredDescr))
}





#======================================================================================================
# Set working directory with data

setwd("C:\\Users\\marco\\Downloads\\agroclimx\\AgroClimExt")

#======================================================================================================
# Read data

dataset_soybean <- read.csv('dataset_soybean.csv')
dataset_maize   <- read.csv('dataset_maize.csv')


#======================================================================================================
# Run models


nculsters <- 20

cl <- makePSOCKcluster(nculsters)
registerDoParallel(cl)

#======================================================================================================
# Soybean

#======================================================================================================
# Create empty vectors and lists

n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

RFModel_soybean <- vector('list', list_length)
var_importance <-  vector('list', list_length)
performance <-  vector('list', list_length)

for(i in 1:list_length){

  #======================================================================================================
  # Sub-set dataset

  dataset_sub <- dataset_soybean %>%
    subset(UF == n_models[i,]$UF &
           dataset == n_models[i,]$dataset)

  #======================================================================================================
  # Pre-process

  filteredDescr <- filter(dataset_sub)

  #======================================================================================================
  # Temporal k-fold to avoid overfitting

  #------------------------------------------------------
  # Window 30% of the number of years

  window = round(length(unique(dataset_sub$Year))*0.30,digits=0)

  #======================================================================================================
  # Temporal k-fold to avoid overfitting

  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 8,
                       horizon = 2,
                       fixedWindow = TRUE,
                       savePredictions = T,
                       search = "random"
  )


  #======================================================================================================
  # Train (80%) test (20%) split

  year_max <- max(dataset_sub$Year)
  cut_year <- year_max-length(round(unique(dataset_sub$Year),digits=0))*0.2
  cut_year <- round(cut_year,digits = 0)

  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)


  #======================================================================================================
  # Train model

  tuneLength <- 20

  model <- train(Yield_corrected ~ .,
                 data = train[filteredDescr],
                 method = "rf",
                 tuneLength=20,
                 preProc = c("center", "scale"),
                 trControl = ctrl
  )


  #------------------------------------------------------
  # Variable importance


  RFModel_soybean[[i]] <- model
  var_importance[[i]] <-  varImp(model)
  performance[[i]] <-  postResample(predict(model,test),test$Yield_corrected)

}


#======================================================================================================
# Maize

#======================================================================================================
# Create empty vectors and lists


n_models <- dataset_maize[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

RFModel_maize <- vector('list', list_length)
var_importance_maize <-  vector('list', list_length)
performance_maize <-  vector('list', list_length)

for(i in 1:list_length){

  #======================================================================================================
  # Sub-set dataset

  dataset_sub <- dataset_maize %>%
    dplyr::filter(UF == n_models[i,]$UF,
           dataset == n_models[i,]$dataset) %>%
    dplyr::filter(Year >= 1997)

  #======================================================================================================
  # Pre-process

  filteredDescr <- filter(dataset_sub)

  #======================================================================================================
  # Temporal k-fold to avoid overfitting

  #------------------------------------------------------
  # Window 30% of the number of years

  window = round(length(unique(dataset_sub$Year))*0.30,digits=0)

  # Horizon 20% of the window
  horizon = round(window*0.20,digits=0)+1


  #======================================================================================================
  # Temporal k-fold to avoid overfitting

  ctrl <- trainControl(method = "timeslice",
                       initialWindow = window,
                       horizon = horizon,
                       fixedWindow = TRUE,
                       savePredictions = T,
                       search = "random"
  )


  #======================================================================================================
  # Train (80%) test (20%) split

  year_max <- max(dataset_sub$Year)
  cut_year <- year_max-length(round(unique(dataset_sub$Year),digits=0))*0.2
  cut_year <- round(cut_year,digits = 0)

  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)


  #======================================================================================================
  # Train model

  tuneLength <- 20

  model <- train(Yield_corrected ~ .,
                 data = train[filteredDescr],
                 method = "rf",
                 tuneLength=20,
                 preProc = c("center", "scale"),
                 trControl = ctrl
  )

  #------------------------------------------------------
  # Add model to a list

  RFModel_maize[[i]] <- model
  var_importance_maize[[i]] <-  varImp(model)
  performance_maize[[i]] <-  postResample(predict(model,test),test$Yield_corrected)



}



## When you are done:
stopCluster(cl)


#======================================================================================================
# Evaluate results

#------------------------------------------------------
# Model performance
library(envalysis)
library(ggpubr)
library(ggthemes)

n_models_soybean <- dataset_soybean[,c("UF","dataset")] %>% unique()
n_models_maize <- dataset_maize[,c("UF","dataset")] %>% unique()

# Soybean
do.call("rbind",performance) %>%
  as.data.frame() %>%
  cbind(n_models_soybean) %>%
  pivot_longer(!c(dataset,UF),names_to="models",values_to="metrics") %>%
  mutate(metrics = ifelse(metrics > 100,metrics/1000,metrics)) %>%
  ggplot(aes(dataset,metrics))+
  geom_boxplot()+
  facet_wrap(~models,scales="free")+
  labs(
    title = "Soybean",
    #subtitle = "Model performance",
    #caption = "Data from the 1974 Motor Trend US magazine.",
    x = "Dataset",
    y = "",
    tag = "(A)"
  ) +
ggthemes::theme_calc() -> p1

# Maize


do.call("rbind",performance_maize) %>%
  as.data.frame() %>%
  cbind(n_models_maize) %>%
  pivot_longer(!c(dataset,UF),names_to="models",values_to="metrics") %>%
  mutate(metrics = ifelse(metrics > 100,metrics/1000,metrics)) %>%
  ggplot(aes(dataset,metrics))+
  geom_boxplot()+
  facet_wrap(~models,scales="free")+
  labs(
    title = "Maize",
    #subtitle = "Model performance",
    #caption = "Data from the 1974 Motor Trend US magazine.",
    x = "Dataset",
    y = "",
    tag = "(B)"
  ) +
  ggthemes::theme_calc() -> p2

ggarrange(p1,p2,ncol=1)

#------------------------------------------------------
# Variable Importance


List_of_hazards <- data.frame(
  hazards = c('mean_air_temp',
  'mean_air_temp',
  'extreme_heat',
  'extreme_heat',
  'extreme_heat',
  'extreme_heat',
  'extreme_heat',
  'extreme_heat',
  'cold_spell',
  'cold_spell',
  'extreme_heat',
  'extreme_heat',
  'mean_precipitation',
  'heavy_precipitation',
  'heavy_precipitation',
  'drought',
  'drought',
  'drought',
  'drought'),
  feature = c('temp',
              'dtr',
              'tx90p',
              'tn90p',
              'su',
              'tr',
              'txx',
              'tnx',
              'tx10p',
              'tn10p',
              'tnn',
              'txn',
              'prcptot',
              'r10mm',
              'r20mm',
              'spei_3month',
              'spei_6month',
              'spi_3month',
              'spi_6month'))

# Soybean
n_models_soybean %>%
  mutate(
    id = 1:n()
  ) -> n_models_soybean

count.id <- 0
lapply(1:nrow(n_models),
       function(i){
         Variables <- rownames(var_importance[[i]]$importance)
         var_importance <- var_importance[[i]]$importance
         var_importance$Variables <- Variables
         var_importance <- var_importance %>%
           arrange(desc(Overall))
         count.id <<- count.id+1
         var_importance$id <-  count.id
         var_importance$rank <- 1:nrow(var_importance)
         return(var_importance)
       }) -> var_importance_soybean


do.call("rbind",var_importance_soybean) %>%
  merge(n_models_soybean,by='id') -> var_importance_soybean2


var_importance_soybean2 %>%
  mutate(
    month = substr(Variables,nchar(Variables)-2,nchar(Variables)),
    feature = substr(Variables,1,nchar(Variables)-4)

  ) %>% merge(
    List_of_hazards,by="feature"
  ) -> var_importance_soybean2

var_importance_soybean2 %>%
  group_by(UF,month,dataset) %>%
  summarise(hazards = hazards[Overall == max(Overall)],
            feature = feature[Overall == max(Overall)],
            Overall = max(Overall)) %>% View()

write.csv(var_importance_soybean2,paste0(output, "var_importance_soybean.csv"),row.names = F)


# Maize
n_models_maize %>%
  mutate(
    id = 1:n()
  ) -> n_models_maize

count.id <- 0
lapply(1:nrow(n_models_maize),
       function(i){
         Variables <- rownames(var_importance_maize[[i]]$importance)
         var_importance_maize1 <- var_importance_maize[[i]]$importance
         var_importance_maize1$Variables <- Variables
         var_importance_maize1 <- var_importance_maize1 %>%
           arrange(desc(Overall))
         count.id <<- count.id+1
         var_importance_maize1$id <-  count.id
         var_importance_maize1$rank <- 1:nrow(var_importance_maize1)
         return(var_importance_maize1)
       }) -> var_importance_maize1

do.call("rbind",var_importance_maize1) %>%
  merge(n_models_maize,by='id') -> var_importance_maize1

var_importance_maize1 %>%
  mutate(
    month = substr(Variables,nchar(Variables)-2,nchar(Variables)),
    feature = substr(Variables,1,nchar(Variables)-4)

  ) %>% merge(
    List_of_hazards,by="feature"
  ) -> var_importance_maize1

write.csv(var_importance_maize1,paste0(output,"var_importance_maize.csv"),row.names = F)

