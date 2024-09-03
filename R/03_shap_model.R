#  SHAPELY additive models for explaining crop yield losses

# The objective of this script is to run RF model
# for explaining the impact of climate extremes on crop yields

# Marcos Benso, Ago 2024


#======================================================================================================
# Load packages

library(xgboost)
library(shapr)
library(caret)
library(tidyverse)
library(ggpubr)
library(DALEX)
library(ranger)

library(shapviz)
library(treeshap)
library(ggplot2)
library(xgboost)

set.seed(123)

#======================================================================================================
# Set working directory with data

setwd("C:\\Users\\marco\\Downloads\\agroclimx\\AgroClimExt")
output <- "C:\\Projetos\\ClimateImpactML\\Data\\Output\\Step1\\"

#======================================================================================================
# Read data

dataset_soybean <- read.csv('dataset_soybean.csv')
dataset_maize   <- read.csv('dataset_maize.csv')

#======================================================================================================
# Selected indices

selected_indices_soybean <- read.csv(paste0(output, "var_importance_soybean.csv"))
selected_indices_maize <- read.csv(paste0(output, "var_importance_maize.csv"))


#======================================================================================================
# Soybean

n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

soybean_shap <- c()
soybean_test <- c()

for(i in 1:list_length){

  #======================================================================================================
  # Create empty vectors and lists
  n_indices <- 10

  n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()


  list_length <- n_models %>% nrow()

  #======================================================================================================
  # Subset

  dataset_sub <- dataset_soybean %>%
    subset(UF == n_models[i,]$UF &
             dataset == n_models[i,]$dataset)


  selected_indices_soybean %>%
    subset(UF == n_models[i,]$UF &
             dataset == n_models[i,]$dataset) %>%
    arrange(desc(Overall)) %>%
    subset(rank <= n_indices) %>%
    dplyr::select(Variables) -> variables

  #======================================================================================================
  # Train (80%) test (20%) split

  year_max <- max(dataset_sub$Year)
  cut_year <- year_max-length(round(unique(dataset_sub$Year),digits=0))*0.2
  cut_year <- round(cut_year,digits = 0)

  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)


  fit <- ranger(Yield_corrected ~ .,
                data = train[c("Yield_corrected",variables$Variables)])

  postResample(predict(fit,test)$predict,test$Yield_corrected)


  unified_model <- ranger.unify(fit, test[variables$Variables])


  shaps <- treeshap(unified_model, test[variables$Variables],
                    interactions = TRUE)

  soybean_shap[[i]] <- shaps
  soybean_test[[i]] <- test


}


#======================================================================================================
# Maize

n_models <- dataset_maize[,c("UF","dataset")] %>% unique()

list_length <- n_models %>% nrow()

maize_shap <- c()
maize_test <- c()

for(i in 1:list_length){

  #======================================================================================================
  # Create empty vectors and lists
  n_indices <- 10

  n_models <- dataset_maize[,c("UF","dataset")] %>% unique()


  list_length <- n_models %>% nrow()

  #======================================================================================================
  # Subset

  dataset_sub <- dataset_maize %>%
    subset(UF == n_models[i,]$UF &
             dataset == n_models[i,]$dataset)


  selected_indices_maize %>%
    subset(UF == n_models[i,]$UF &
             dataset == n_models[i,]$dataset) %>%
    arrange(desc(Overall)) %>%
    subset(rank <= n_indices) %>%
    dplyr::select(Variables) -> variables

  #======================================================================================================
  # Train (80%) test (20%) split

  year_max <- max(dataset_sub$Year)
  cut_year <- year_max-length(round(unique(dataset_sub$Year),digits=0))*0.2
  cut_year <- round(cut_year,digits = 0)

  train <- dataset_sub %>% subset(Year <= cut_year)
  test  <- dataset_sub %>% subset(Year > cut_year)

  fit <- ranger(Yield_corrected ~ .,
                data = train[c("Yield_corrected",variables$Variables)])



  postResample(predict(fit,test)$predict,test$Yield_corrected)


  unified_model <- ranger.unify(fit, test[variables$Variables])


  shaps <- treeshap(unified_model, test[variables$Variables],
                    interactions = TRUE)

  maize_shap[[i]] <- shaps
  maize_test[[i]] <- test

}



#-------------------------------------------------------------------------------
# Waterfall plot

n_models <- dataset_soybean[,c("UF","dataset")] %>% unique()

n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "PR")

IBGE_PR <- n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "PR" & dataset == "IBGE") |>
  select(n) |> as.numeric()

DERAL_PR <- n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "PR" & dataset == "DERAL") |>
  select(n) |> as.numeric()

GDHY_PR <- n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "PR" & dataset == "GDHY") |>
  select(n) |> as.numeric()


#IBGE PR n = 5 & Borrazópolis 134  Uniflor 1077

soybean_test[[IBGE_PR]] |>
  mutate(n = 1:n()) |>
  subset(City %in% c("borrazopolis","uniflor") & Year == 2019) |>
  select(c(City,Year,n,Yield_corrected)) |>
  mutate()





shp_IBGE <- shapviz(soybean_shap[[IBGE_PR]],
                    baseline = mean(soybean_test[[IBGE_PR]]$Yield_corrected),
                    X = soybean_test[[IBGE_PR]])


#Deral PR n = 7 & Borrazópolis Uniflor

shp_Deral <- shapviz(soybean_shap[[DERAL_PR]],
               baseline = mean(soybean_test[[DERAL_PR]]$Yield_corrected),
               X = soybean_test[[DERAL_PR]])

soybean_test[[DERAL_PR]] |>
  mutate(n = 1:n()) |>
  subset(City %in% c("borrazopolis","uniflor") & Year == 2019) |>
  select(c(City,Year,n,Yield))


shp_GDHY <- shapviz(soybean_shap[[GDHY_PR]],
                     baseline = mean(soybean_test[[GDHY_PR]]$Yield_corrected),
                     X = soybean_test[[GDHY_PR]])








#----------------------------------------------------

ggpubr::ggarrange(

  # (A) Borrazópolis (83) - Deral (7)

  sv_waterfall(shp_Deral,row_id = 83)+
    labs(title = "City Borrazópolis - PR (Deral)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[7]]$Yield_corrected[83],digits=2),
                          "| Error ", round(predict(soybean_shap[[7]]$unified_model,soybean_test[[7]])[83]-soybean_test[[7]]$Yield_corrected[83],digits=2),
                          " ton/ha"),
         tag = '(A)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (B) Uniflor (672) - Deral (7)
  sv_waterfall(shp_Deral,row_id = 672)+
    labs(title = "City Uniflor - PR (Deral)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[7]]$Yield_corrected[672],digits=2),
                          "| Error ", round(predict(soybean_shap[[7]]$unified_model,soybean_test[[7]])[672]-soybean_test[[7]]$Yield_corrected[672],digits=2),
                          " ton/ha"),
         tag = '(B)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (C) Borrazópolis (134) - IBGE (5)
  sv_waterfall(shp_IBGE,row_id = 134)+
    labs(title = "City Borrazópolis - PR (IBGE)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[5]]$Yield_corrected[134],digits=2),
                          "| Error ", round(predict(soybean_shap[[5]]$unified_model,soybean_test[[5]])[134]-soybean_test[[5]]$Yield_corrected[134],digits=2),
                          " ton/ha"),
         tag = '(C)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (D) Uniflor (1077) - IBGE (5)
  sv_waterfall(shp_IBGE,row_id = 1077)+
    labs(title = "City Uniflor - PR (IBGE)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[5]]$Yield_corrected[1077],digits=2),
                          "| Error ", round(predict(soybean_shap[[5]]$unified_model,soybean_test[[5]])[1077]-soybean_test[[5]]$Yield_corrected[1077],digits=2),
                          " ton/ha"),
         tag = '(D)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),
  ncol=2,
  nrow=2
)

ggsave("explanation-pr-2019.png",
       height = 10, width = 15)




#----------------------------------------------------

IBGE_SP <- n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "SP" & dataset == "IBGE") |>
  select(n) |> as.numeric()

GDHY_SP <- n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "SP" & dataset == "GDHY") |>
  select(n) |> as.numeric()


shp_IBGE_SP <- shapviz(soybean_shap[[IBGE_SP]],
                     baseline = mean(soybean_test[[IBGE_SP]]$Yield_corrected),
                     X = soybean_test[[IBGE_SP]])


shp_GDHY <- shapviz(soybean_shap[[GDHY_SP]],
                     baseline = mean(soybean_test[[GDHY_SP]]$Yield_corrected),
                     X = soybean_test[[GDHY_SP]])


soybean_test[[IBGE_SP]] |>
  mutate(n = 1:n()) |>
  subset(City %in% c("orlandia","pontal") & Year == 2015) |>
  select(c(City,Year,n,Yield))

soybean_test[[4]] |>
  mutate(n = 1:n()) |>
  subset(City %in% c("orlandia","pontal") & Year == 2015) |>
  select(c(City,Year,n,Yield))

ggpubr::ggarrange(

  # (A) Orlândia (261) - IBGE (3)

  sv_waterfall(shp_IBGE_SP,row_id = 261)+
    labs(title = "City Orlândia - SP (IBGE)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[IBGE_SP]]$Yield_corrected[261],digits=2),
                          "| Error ", round(predict(soybean_shap[[IBGE_SP]]$unified_model,soybean_test[[3]])[261]-soybean_test[[3]]$Yield_corrected[261],digits=2),
                          " ton/ha"),
         tag = '(A)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (B) Pontal (316)  - IBGE (3)
  sv_waterfall(shp_IBGE_SP,row_id = 316)+
    labs(title = "City Pontal - SP (IBGE)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[IBGE_SP]]$Yield_corrected[316],digits=2),
                          "| Error ", round(predict(soybean_shap[[IBGE_SP]]$unified_model,soybean_test[[IBGE_SP]])[316]-soybean_test[[3]]$Yield_corrected[316],digits=2),
                          " ton/ha"),
         tag = '(B)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (C) Orlândia (134) - GDHY (4)
  sv_waterfall(shp_GDHY,row_id = 134)+
    labs(title = "City Borrazópolis - PR (GDHY)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[shp_GDHY]]$Yield_corrected[134],digits=2),
                          "| Error ", round(predict(soybean_shap[[shp_GDHY]]$unified_model,soybean_test[[shp_GDHY]])[134]-soybean_test[[4]]$Yield_corrected[134],digits=2),
                          " ton/ha"),
         tag = '(C)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),

  # (D) Pontal (1077) - IBGE (4)
  sv_waterfall(shp_GDHY,row_id = 1077)+
    labs(title = "City Uniflor - PR (IBGE)",
         subtitle = paste("Actual Yield ",
                          round(soybean_test[[4]]$Yield_corrected[1077],digits=2),
                          "| Error ", round(predict(soybean_shap[[4]]$unified_model,soybean_test[[4]])[1077]-soybean_test[[4]]$Yield_corrected[1077],digits=2),
                          " ton/ha"),
         tag = '(D)'
    )+
    xlab("SHAP Values ton/ha")+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    ),
  ncol=2,
  nrow=2
)

ggsave("explanation-sp-2015.png",
       height = 10, width = 15)



#-------------------------------------------------------------------------------
# Dependence plot

n_models |>
  mutate(n = 1:n()) |>
  subset(UF == "RS") |>
  select(n) |>
  as.vector() -> dataset_RS


shp_RS_IBGE <- shapviz(soybean_shap[[dataset_RS$n[1]]],
               baseline = mean(soybean_test[[dataset_RS$n[1]]]$Yield_corrected),
               X = soybean_test[[dataset_RS$n[1]]])

shp_RS_GDHY <- shapviz(soybean_shap[[dataset_RS$n[2]]],
                          baseline = mean(soybean_test[[dataset_RS$n[2]]]$Yield_corrected),
                          X = soybean_test[[dataset_RS$n[2]]])

ggpubr::ggarrange(

  # 3-month SPEI in Feb
  data.frame(spei_3month_Feb = shp_RS_IBGE$X$spei_3month_Feb,
             SHAP_values = as.data.frame(shp_RS_IBGE$S)$spei_3month_Feb) |>
    ggplot(aes(spei_3month_Feb,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (IBGE)",
         tag = '(A)'
    )+
    ylim(-600,400)+
    ylab("SHAP values \n ton/ha"),
  data.frame(spei_3month_Feb = shp_RS_GDHY$X$spei_3month_Feb,
             SHAP_values = as.data.frame(shp_RS_GDHY$S)$spei_3month_Feb) |>
    ggplot(aes(spei_3month_Feb,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (GDHY)",
         tag = '(B)'
    )+
    ylim(-0.6,0.4)+
    ylab("SHAP values \n ton/ha"),

  # PRECTOT FEB
  data.frame(prcptot_Feb = shp_RS_IBGE$X$prcptot_Feb,
             SHAP_values = as.data.frame(shp_RS_IBGE$S)$prcptot_Feb) |>
    ggplot(aes(prcptot_Feb,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (IBGE)",
         tag = '(C)'
    )+
    ylim(-600,400)+
    ylab("SHAP values \n ton/ha"),
  data.frame(prcptot_Feb = shp_RS_GDHY$X$prcptot_Feb,
             SHAP_values = as.data.frame(shp_RS_GDHY$S)$prcptot_Feb) |>
    ggplot(aes(prcptot_Feb,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (GDHY)"
         ,
         tag = '(D)'
    )+
    ylim(-0.6,0.4)+
    ylab("SHAP values \n ton/ha"),

  # PRECTOT DEC
  data.frame(prcptot_Dec = shp_RS_IBGE$X$prcptot_Dec,
             SHAP_values = as.data.frame(shp_RS_IBGE$S)$prcptot_Dec) |>
    ggplot(aes(prcptot_Dec,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (IBGE)",
         tag = '(E)'
    )+
    ylim(-600,400)+
    ylab("SHAP values \n ton/ha"),

  data.frame(prcptot_Dec = shp_RS_GDHY$X$prcptot_Dec,
             SHAP_values = as.data.frame(shp_RS_GDHY$S)$prcptot_Dec) |>
    ggplot(aes(prcptot_Dec,SHAP_values))+
    geom_jitter()+
    geom_hline(yintercept = 0)+
    labs(title = "Soybean - RS (GDHY)",
         tag = '(F)'
    )+
    ylim(-0.6,0.4)+
    ylab("SHAP values \n ton/ha"),
  ncol=2,
  nrow=3
)

ggsave("dependence_RS.png",
       height = 10, width = 15)

#-------------------------------------------------------------------------------
# Interaction plot


ggpubr::ggarrange(
  sv_dependence(shp_Deral,"spei_3month_Feb","prcptot_Dec",interactions = T
  )+
    labs(title = "Interactions between Precipitation in December \n and 3-month SPEI in February in Paraná",
         subtitle = "Data source: Deral",
         tag = "(A)"
    )+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    )+
    scale_colour_gradient2(
      low = "#d53e4f",
      mid = "#ffffbf",
      high = "#3288bd",
      midpoint=200
    ),

  sv_dependence(shp_IBGE,"spei_3month_Feb","prcptot_Dec",interactions = T
  )+
    labs(title = "Interactions between Precipitation in December \n and 3-month SPEI in February in Paraná",
         subtitle = "Data source: IBGE",
         tag = "(B)"
    )+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust = 0)
    )+
    scale_colour_gradient2(
      low = "#d53e4f",
      mid = "#ffffbf",
      high = "#3288bd",
      midpoint=200
    ),


  sv_dependence(shp_Deral,"prcptot_Dec","tnx_Feb",interactions = T
  )+
    labs(title = "Interactions between Precipitation in December \n and Extreme Temperature in February in Paraná",
         subtitle = "Data source: Deral",
         tag = "(C)"
    )+
    theme(
      plot.title = element_text(color = "black",
                                size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust =0)
    )+
    scale_colour_gradient2(
      low = "#3288bd",
      mid = "#ffffbf",
      high = "#d53e4f",
      midpoint=27
    ),
  sv_dependence(shp_IBGE,"prcptot_Dec","tnx_Feb",interactions = T
  )+
    labs(title = "Interactions between Precipitation in December \n and Extreme Temperature in February in Paraná",
         subtitle = "Data source: IBGE",
         tag = "(D)"
    )+
    theme(
      plot.title = element_text(color = "black",
                                size = 12, face = "bold",hjust = 0, vjust = 0),
      plot.subtitle = element_text(color = "black",hjust = 0, vjust =0)
    )+
    scale_colour_gradient2(
      low = "#3288bd",
      mid = "#ffffbf",
      high = "#d53e4f",
      midpoint=27
    ),
  ncol=2,
  nrow=2
)

ggsave("interaction_PR.png",
       height = 10, width = 15)

