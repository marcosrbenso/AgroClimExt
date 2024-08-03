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

filter <-function(merged_dataset){
  
  #------------------------------------------------------
  # Remove near Zero variance features
  nzv <- nearZeroVar(merged_dataset %>% ungroup() %>%
                       select(-c(Year,flag,block_id,
                                 code_muni,
                                 City,UF,Yield,code_state,lat,geom,
                                 clay,silt,sand)))
  
  
  
  filteredDescr <- merged_dataset[, -nzv] %>% ungroup() %>%
    select(-c(Year,flag,block_id,
              code_muni,
              City,UF,Yield,code_state,lat,geom,
              clay,silt,sand))
  
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

var_importance <- c()

n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

RFModel_soybean <- vector('list', list_length)

for(i in 1:list_length){
  
  
  
  #======================================================================================================
  # Sub-set dataset
  
  dataset_sub <- dataset_soybean %>%
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
  horizon = round(window*0.20,digits=0)
  
  
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
  
  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)
  
  
  #======================================================================================================
  # Train model
  
  tuneLength <- 20
  
  model <- train(Yield_detrended ~ .,
                       data = train[filteredDescr],
                       method = "cforest",
                       tuneLength=20,
                       metric="RMSE",
                       maximize=F,
                       preProc = c("center", "scale"),
                       trControl = ctrl
  )
  
  #------------------------------------------------------
  # Add model to a list
  
  
  RFModel_Vector[[i]] <- model
  
  #======================================================================================================
  # Variable importance
  
  var_importance_new <- varImp(model)
  
  png(filename=paste(output,"Soybean","Variable Importance",uf,dataset,sep='_'))
  plot(varImp(model), top = 20,
       main=paste("Variable Importance",uf,dataset,"Soybean"))
  dev.off()
  
  
}


#======================================================================================================
# Maize

#======================================================================================================
# Create empty vectors and lists

var_importance <- c()

n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

RFModel_soybean <- vector('list', list_length)


for(i in 1:list_length){
  
  
  
  #======================================================================================================
  # Sub-set dataset
  
  dataset_sub <- dataset_soybean %>%
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
  horizon = round(window*0.20,digits=0)
  
  
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
  
  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)
  
  
  #======================================================================================================
  # Train model
  
  tuneLength <- 20
  
  model <- train(Yield_detrended ~ .,
                 data = train[filteredDescr],
                 method = "cforest",
                 tuneLength=20,
                 metric="RMSE",
                 maximize=F,
                 preProc = c("center", "scale"),
                 trControl = ctrl
  )
  
  #------------------------------------------------------
  # Add model to a list
  
  
  RFModel_Vector[[i]] <- model
  
  #======================================================================================================
  # Variable importance
  
  var_importance_new <- varImp(model)
  
  png(filename=paste(output,"Maize","Variable Importance",uf,dataset,sep='_'))
  plot(varImp(model), top = 20,
       main=paste("Variable Importance",uf,dataset,"Maize"))
  dev.off()
  
  
}



## When you are done:
stopCluster(cl)
