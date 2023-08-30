library(ggcorrplot)
library(factoextra)
library(ggpubr)
library(caret)
library(ranger)
library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(shapviz)

data.soja.x <- read.csv("dadossoja.csv")
data.milho.x <- read.csv("dadosmilho.csv")



##### Correlation plot ######

maize <- data.milho.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                        'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                        "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                        "temp_lag1","temp_lag2","temp_lag3","temp_lag4",
                        "yield","year","city_dummy")]

soybean <- data.soja.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                         'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                         "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                         "temp_lag1","temp_lag2","temp_lag3","temp_lag4","yield",
                         "city_dummy",
                         "year")]

p.mat.maize   <- cor_pmat(maize)
p.mat.soybean <- cor_pmat(soybean)

cor_plot_maize <- ggcorrplot(cor(maize),method="square",type='lower',
                             title="Correlogram - Maize",
                             legend.title = "Pearson \n Corr",
                             lab=T, lab_col = "black", lab_size = 3.5,
                             ggtheme = theme_grey,outline.color = "black",
                             p.mat = p.mat.maize,
                             sig.level = 0.01,
                             insig = "blank")


cor_plot_soybean <- ggcorrplot(cor(soybean),method="square",type='lower',
                             title="Correlogram - Soybean",
                             legend.title = "Pearson \n Corr",
                             lab=T, lab_col = "black", lab_size = 3.5,
                             ggtheme = theme_grey,outline.color = "black",
                             p.mat = p.mat.soybean,
                             sig.level = 0.01,
                             insig = "blank")

cor_plot_maize
ggsave("cor_plot_maize.png",width = 10, height = 10)
cor_plot_soybean
ggsave("cor_plot_soybean.png",width = 10, height = 10)


##### PCA #######



pca.soybean <- prcomp(soybean[,colnames(soybean) != "yield"],scale=T)
pca.maize   <- prcomp(maize[,colnames(maize) != "yield"],scale=T)

ggarrange(annotate_figure(ggarrange(fviz_eig(pca.soybean,
                                      addlabels = T,
                                      ylim=c(0,50)),

                             fviz_pca_biplot(pca.soybean,
                                             label="var",
                                             habillage = as.factor(ifelse(data.soja.x$yield < 0, "Loss","Normal")))),
                   top = text_grob("PCA - soybean", size = 14)),


annotate_figure(ggarrange(fviz_eig(pca.maize,
                                   addlabels = T,
                                   ylim=c(0,50)),

                          fviz_pca_biplot(pca.maize,
                                          label="var",
                                          habillage = as.factor(ifelse(data.milho.x$yield < 0, "Loss","Normal")))),
                top = text_grob("PCA - maize", size = 14))
,

ncol=1,labels=c("A","B")
)
ggsave("pca_plot.png", width = 10, height = 10)


#### RFE ######

library("parallel")
library("doParallel")

Mycluster = makeCluster(detectCores()-1)
registerDoParallel(Mycluster)

rf_ctrl <- rfeControl(
  functions = rfFuncs,
  method="repeatedcv",
  number = 10,
  repeats = 3,
  allowParallel = T
)

rfe.soja <- rfe(
  yield ~ .,
  data = soybean,
  sizes = c(1:25),
  rfeControl = rf_ctrl,
  number = 1000
)

rfe.milho <- rfe(
  yield ~ .,
  data = maize,
  sizes = c(1:25),
  rfeControl = rf_ctrl,
  number = 1000
)

stopcluster(cl)

ggarrange(data.frame(Var = factor(rownames(varImp(rfe.soja))),
                     Importance = varImp(rfe.soja)$Overall) %>%
            mutate(Var = forcats::fct_reorder(Var,Importance,.desc=F)) %>%
            ggplot(aes(Importance,Var))+
            geom_bar(stat='identity',fill="#fc8d59",col="#af8dc3")+
            xlab("Importance (%)")+ylab("")+ggtitle("Soybean"),

          data.frame(Var = factor(rownames(varImp(rfe.milho))),
                     Importance = varImp(rfe.milho)$Overall) %>%
            mutate(Var = forcats::fct_reorder(Var,Importance,.desc=F)) %>%
            ggplot(aes(Importance,Var))+
            geom_bar(stat='identity',fill="#91cf60",col="#7fbf7b")+
            xlab("Importance (%)")+ylab("")+ggtitle("Maize"),
          labels=c("A","B")

          )
ggsave("varImp.png",width = 10, height = 5)


##### Compare methods #####

var_soybean_cor <- names(cor(soybean)[23,p.mat.soybean[23,]<0.001])[names(cor(soybean)[23,p.mat.soybean[23,]<0.001]) != "yield"]
var_maize_cor <- names(cor(maize)[23,p.mat.maize[23,]<0.001])[-14][names(cor(maize)[23,p.mat.maize[23,]<0.001])[-14] != "yield"]


best.pca <- function(res.pca){
  pca_results <- as.data.frame(res.pca$rotation)
  pca_results$var <- rownames(pca_results)
  res <- rep(1,10)
  for(i in 1:10){
    if(abs(max(pca_results[,i])) > abs(min(pca_results[,i]))){
      res[i] <- rownames(pca_results[pca_results[,i]==max(pca_results[,i]),])
    }
    else{
      res[i] <- rownames(pca_results[pca_results[,i]==min(pca_results[,i]),])
    }
  }
  return(res)
}


var_soybean_pca <- best.pca(pca.maize)
var_maize_pca <- best.pca(pca.soybean)

var_soybean_rfe <- caret::predictors(rfe.soja)
var_maize_rfe <- caret::predictors(rfe.milho)


rf.models.soybean <- lapply(
  list(all = c("cdd","cwd","prec","rx1day","rx5day","sdii",
               'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
               "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
               "temp_lag1","temp_lag2","temp_lag3","temp_lag4",
               "year","city_dummy"),
       cor = var_soybean_cor,
       PCA = unique(var_soybean_pca),
       RFE = var_soybean_rfe)
  ,
  function(x) {

    train(
      yield ~ .,
      data = data.soja.x[data.soja.x$year %in% 1997:2015,
                         c(x,"yield")],
      method='rf',
      length = 15,
      preProc =  c("center", "scale"),
      trControl = caret::trainControl(
        method="cv",
        number = 10
      )
    )
  }
)

rf.models.maize <- lapply(
  list(all = c("cdd","cwd","prec","rx1day","rx5day","sdii",
               'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
               "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
               "temp_lag1","temp_lag2","temp_lag3","temp_lag4",
               "year","city_dummy"),
       cor = var_maize_cor,
       PCA = unique(var_maize_pca),
       RFE = var_maize_rfe)
  ,
  function(x) {
    train(
      yield ~ .,
      data = data.milho.x[data.milho.x$year %in% 1997:2015,
                         c(x,"yield")],
      method='rf',
      length = 15,
      preProc =  c("center", "scale"),
      trControl = caret::trainControl(
        method="cv",
        number = 10
      )
    )
  }
)


ggarrange(

do.call("rbind",lapply(rf.models.soybean,function(x){
  postResample(predict(x,data.soja.x[data.soja.x$year %in% 2016:2021,]),
               data.soja.x[data.soja.x$year %in% 2016:2021,]$yield)
}
)) %>%
  as.data.frame() %>%
  mutate(Names = c("All","Cor","PCA","RFE")) %>%
  ggplot(aes(Names,Rsquared))+geom_bar(stat="identity",fill="#fc8d59",col="#af8dc3")+
  xlab("")+ggtitle("Soybean")+
  ylim(c(0,0.4)),

do.call("rbind",lapply(rf.models.maize,function(x){
  postResample(predict(x,data.milho.x[data.milho.x$year %in% 2016:2021,]),
               data.milho.x[data.milho.x$year %in% 2016:2021,]$yield)
}
)) %>%
  as.data.frame() %>%
  mutate(Names = c("All","Cor","PCA","RFE")) %>%
  ggplot(aes(Names,Rsquared))+geom_bar(stat="identity",fill="#91cf60",col="#7fbf7b")+
  xlab("")+ggtitle("Maize")+
  ylim(c(0,0.4)),
labels=c("A","B"))

ggsave("modelperformance.png",width = 6, height = 4)

##### Post-hoc analysis ######

y_var <- "yield"

x_train_soybean <- as.matrix(data.soja.x[data.soja.x$year %in% 1997:2015, c(var_soybean_rfe,y_var)])
y_train_soybean <- as.matrix(data.soja.x[data.soja.x$year %in% 1997:2015, c(y_var)])
x_test_soybean <- as.matrix(data.soja.x[data.soja.x$year %in% 2016:2020, c(var_soybean_rfe,y_var)])

x_train_maize <- as.matrix(data.milho.x[data.milho.x$year %in% 1997:2015, c(var_maize_rfe,y_var)])
y_train_maize <- as.matrix(data.milho.x[data.milho.x$year %in% 1997:2015, c(y_var)])
x_test_maize <- as.matrix(data.milho.x[data.milho.x$year %in% 2016:2020, c(var_maize_rfe,y_var)])


explanation_soybean <- shapr::explain(
  x = x_test_soybean,
  approach = "empirical",
  explainer = shapr::shapr(x_train_soybean, ranger(
    yield ~ .,
    data = x_train_soybean
  )),
  prediction_zero = mean(x_train_soybean[,"yield"])
)

save(explanation_soybean,file="explanation_soja.RData")

names(explanation_maize$p)
explanation_soybean$p[explanation_soybean$p<0]


