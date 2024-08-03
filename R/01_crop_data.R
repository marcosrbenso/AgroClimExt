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


#======================================================================================================
# Output file

output <- "C:\\Projetos\\ClimateImpactML\\Data\\Output\\Step1\\"


#======================================================================================================
# Pre-processing function

filter <-function(dataset_sub){
  dataset_sub <- dataset_sub %>% ungroup() %>%
    select(-c(Year,Hetero,tau,
              code_muni,Yield_detrended,
              City,UF,Yield,code_state,lat,geom,
              clay,silt,sand))
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


nculsters <- 20

cl <- makePSOCKcluster(nculsters)
registerDoParallel(cl)




#======================================================================================================
#======================================================================================================
# Run models



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
           dataset == n_models[i,]$dataset) %>%
    subset(Year > 1997)

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
                 metric="RMSE",
                 maximize=F,
                 preProc = c("center", "scale"),
                 trControl = ctrl
  )

  #------------------------------------------------------
  # Add model to a list

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

RFModel_maize[[i]] <- vector('list', list_length)
var_importance_maize[[i]] <-  vector('list', list_length)
performance_maize[[i]] <-  vector('list', list_length)

for(i in 1:list_length){

  #======================================================================================================
  # Sub-set dataset

  dataset_sub <- dataset_maize %>%
    subset(UF == n_models[i,]$UF,
           dataset == n_models[i,]$dataset) %>%
    subset(Year > 1997)

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
                 metric="RMSE",
                 maximize=F,
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
