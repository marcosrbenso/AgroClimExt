library(tidyverse)
library(Hmisc)
library(caret)
library(ggpubr)
library(shapr)
library(ranger)

correlation_matrix <- function(df,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = ""){
  "https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/"

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")

  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}


path <- "/home/marcosrb/AgroClimExt/Output/Indices_raw/"

path <- "/home/marcosrb/AgroClimExt/Output/Indices_2/"

indices <- c("prec","temp","spi","spei","fd","id","tr",
             "txx","txn","tnx","tnn","rx1day","rx5day",
             "sdii","r20mm","cdd","cwd")


cities <- map$code_muni
list.files(path=path,pattern = "spei")


##### Indices ######
lapply(indices,function(x){
  list.files(path=path,pattern = x)
})


##### Models #######

setwd(path)

lapply(as.character(cities),function(x){
  data <- lapply(list.files(path=path,pattern = x),function(x){
    data <- read.csv(paste0(path,x))
    data$X <- as.Date(data$X)
    #data <- data %>% subset(month(X) == 1 & day(X) == 30)
    colnames(data) <- c("Date","Value")
    data$Var <- rep(unlist(str_split(x,pattern = "_")[[1]][[1]]),nrow(data))
    data
  })

  #data <- do.call("rbind",data)

}) -> data.indices


lapply(data.indices,function(x){
  data <- do.call("rbind",x)
  data <- pivot_wider(data,names_from="Var",values_from="Value")
}) -> data.indices



do.call("rbind",data.indices) %>%
  mutate(month = factor(month(Date))) %>%
  group_by(month) %>%
  summarise(Prec = round(mean(prec,na.rm=T),0),
            Temp = round(mean(temp,na.rm=T),0)) %>%
  kable(format = "latex",align="center")

fig_means <- ggarrange(do.call("rbind",data.indices) %>%
                         mutate(month = factor(month(Date))) %>%
                         ggplot(aes(month,prec))+geom_boxplot()+
                         ggtitle("Monthly accumulated precipitation"),
                       do.call("rbind",data.indices) %>%
                         mutate(month = factor(month(Date))) %>%
                         ggplot(aes(month,temp))+geom_boxplot()+
                         ggtitle("Monthly mean temperature"),
                       ncol=1,labels = c("A","B"))

annotate_figure(fig_means, top = text_grob("Monthly precipitation and temperature",
                                                color = "black", face = "bold", size = 14))

data.indices[[1]] %>%
  arrange(Date) %>%
  group_by(year = year(Date),month = month(Date)) %>%
  summarise_all(mean,na.rm=T) %>% ungroup() %>%
  mutate(prec_lag1 = lag(prec,1),
         prec_lag2 = lag(prec,2),
         prec_lag3 = lag(prec,3),
         prec_lag4 = lag(prec,4),
         temp_lag1 = lag(temp,1),
         temp_lag2 = lag(temp,2),
         temp_lag3 = lag(temp,3),
         temp_lag4 = lag(temp,4)
         ) %>%
  subset(month == 11) %>% View()


######### Crop data ###########

crop_yield_anomaly <- function(yield, year){

  model <- lm(yield~year)
  #anom <- (model-mean(model,na.rm=T))/sd(model,na.rm=T)
  return(data.frame(yield = model$residuals,harvest = year))
  #yield = yield + ((max(year)-year)*model$coefficients[2])
  #return(yield)

}


cropdata <- read.csv("/home/marcosrb/AgroClimExt/Input/dadosparana.csv")

culturas <- c("soja (1ª safra)","milho (1ª safra)","milho (1ª safra)","milho (2ª safra)","trigo")


cropdata %>% subset(Município %in% cities) %>%
  filter(is.na(yield)==F) %>%
  arrange(harvest,CULTURA,Município) %>%
  filter(CULTURA %in% culturas) %>%
  group_by(CULTURA,Município) %>% nest() %>%
  mutate(yield_res = map(data,~crop_yield_anomaly(.x$yield,.x$harvest))) -> cropdata.dataset


cropdata %>% subset(Município %in% cities) %>%
  filter(is.na(yield)==F) %>%
  arrange(harvest,CULTURA,Município) %>%
  filter(CULTURA %in% c("soja (1ª safra)","milho (2ª safra)")) %>%
  group_by(CULTURA,Município) %>%
  mutate(yield_res = crop_yield_anomaly(yield,harvest)) %>%
  mutate(yield_res = ifelse(yield_res < mean(yield_res),
                            (mean(yield_res)-yield_res)/mean(yield_res),0)) %>%
  group_by(CULTURA,harvest) %>%
  summarise(value = mean(yield_res)*100,
            sd = sd(yield_res)) %>%
  select(-sd) %>%
  pivot_wider(names_from = "CULTURA",values_from="value") %>% write.csv("losses.csv")
  mutate(CULTURA = ifelse(CULTURA == "soja (1ª safra)","Soybean","Maize")) %>%
  ggplot(aes(harvest,value))+
  geom_bar(stat="identity", fill="skyblue", alpha=0.85)+
  geom_errorbar( aes(x=harvest, ymin=value-sd, ymax=value+sd),
                 width=0.4, colour="orange", alpha=0.9, size=1.3)+
  ylab("Expected loss amount in %")+
  xlab("")+
  facet_wrap(~CULTURA,scales="free")

###### Soja #######

lapply(data.indices,function(x){
  x %>%
    arrange(Date) %>%
    group_by(year = year(Date),month = month(Date)) %>%
    summarise_all(mean,na.rm=T) %>% ungroup() %>%
    mutate(prec_lag1 = lag(prec,1),
           prec_lag2 = lag(prec,2),
           prec_lag3 = lag(prec,3),
           prec_lag4 = lag(prec,4),
           temp_lag1 = lag(temp,1),
           temp_lag2 = lag(temp,2),
           temp_lag3 = lag(temp,3),
           temp_lag4 = lag(temp,4)
    ) %>%
    subset(month == 6) %>%
    mutate(year = ifelse(month %in% c(1,2,3,4),
                         year+0,year+0))
}) -> data.soja

data.soja <- tibble(name_mn = cities,
                    data = data.soja)

data.soja %>%
  unnest(data) %>%
  merge(cropdata.dataset[,-3] %>%
          unnest(yield_res) %>% subset(CULTURA == "milho (2ª safra)"),
        by.x = c("year","name_mn"),by.y = c("harvest","Município")) %>%
  mutate(city_dummy = as.numeric(as.factor(name_mn))) -> data.soja.x


data.soja %>%
  unnest(data) %>%
  merge(soja,
        by.x = c("year","name_mn"),by.y = c("year","City")) %>%
  mutate(city_dummy = as.numeric(as.factor(name_mn))) -> data.soja.x



data.soja.x[c("month","prec","temp","yield","name_mn")] %>%
  mutate(month = as.factor(month)) %>%
  pivot_longer(!c(month,yield,name_mn),names_to='var',values_to="val") %>%
  group_by(var,month,name_mn) %>%
  mutate(cor = cor(yield,val,method="pearson")) %>%
  ggplot(aes(month,cor))+
  geom_boxplot()+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.2)+
  geom_hline(yintercept = -0.5)+
  geom_hline(yintercept = -0.2)+
  ylab("Pearson's corelation")+xlab("")+
  facet_wrap(~var)


####### Variable analysis #########

##### ANOVA #####
library(ggcorrplot)

corr <- correlation_matrix()

cor = round(cor(data.soja.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                               'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                               "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                               "temp_lag1","temp_lag2","temp_lag3","temp_lag4","yield")]),1)

p.mat = cor_pmat(data.soja.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                                'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                                "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                                "temp_lag1","temp_lag2","temp_lag3","temp_lag4","yield")])

ggcorrplot(cor,method="square",type='lower',
           title="Correlogram - Maize",
           legend.title = "Pearson \n Corr",
           lab=T, lab_col = "black", lab_size = 3.5,
           ggtheme = theme_grey,outline.color = "black",
           p.mat = p.mat,
           sig.level = 0.01,
           insig = "blank")

write.csv2(corr, file = "correlation_milho.csv")

##### PCA #######

samples <- data.soja.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                    'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                    "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                    "temp_lag1","temp_lag2","temp_lag3","temp_lag4","city_dummy",
                    "year")]

library(factoextra)
library(kernlab)

samples <- scale(samples)

res.pca <- prcomp(samples,scale=T)
summary(res.pca)

ggpubr::ggarrange(fviz_eig(res.pca,
                           addlabels = T,
                           ylim=c(0,50)),

                  fviz_pca_biplot(res.pca,
                                  label="var",
                                  habillage = as.factor(ifelse(data.soja.x$yield < 0, "Loss","Normal"))))


pca_results <- as.data.frame(res.pca$rotation)
pca_results$var <- rownames(pca_results)

for(i in 1:10){
  if(abs(max(pca_results[,i])) > abs(min(pca_results[,i]))){
    print(c(max(pca_results[,i]),
            pca_results[pca_results[,i]==max(pca_results[,i]),25]))
  }
  else{
    print(c(min(pca_results[,i]),
            pca_results[pca_results[,i]==min(pca_results[,i]),25]))
  }

}



###### Kernel PCA #######

kernel_pca <- kpca(~., data=samples,
                   kernel="rbfdot",
                   features=2)
training_set_pca = as.data.frame(predict(kernel_pca, samples))

(rotated(kernel_pca))
plot()
plot(rotated(kernel_pca),
     col=as.factor(ifelse(data.soja.x$yield < 0, "Loss","Normal")),
     xlab="1st Principal Component",ylab="2nd Principal Component")
emb <- predict(kernel_pca,samples)
points(emb,col=as.integer(ifelse(samples$yield < 0, "Loss","Normal")))

predict(kernel_pca,sample)


####### Models ##########

a <- data.soja.x[,c("cdd","cwd","prec","rx1day","rx5day","sdii",
                    'spei',"spi","temp","tnx","tr","tx10p","txn","txx",
                    "prec_lag1","prec_lag2","prec_lag3","prec_lag4",
                    "temp_lag1","temp_lag2","temp_lag3","temp_lag4","yield","city_dummy","year")]



a <- a %>%
  mutate(yield = cut(yield,4,c("Severe","Low","Medium","High")))

a <- a %>%
  group_by(city_dummy) %>%
  mutate(yield = as.factor(ifelse(yield <= 0,"Loss","Normal")))

training <- a[a$year %in% 1997:2017,]
testing  <- a[a$year %in% 2018:2020,]


ctrl <- caret::trainControl(
  method="cv",
  number = 10
)

rf_ctrl <- rfeControl(
  functions = rfFuncs,
  method="repeatedcv",
  number = 10,
  repeats = 3,
  allowParallel = T
)


library("parallel")
library("doParallel")

Mycluster = makeCluster(detectCores()-1)
registerDoParallel(Mycluster)


###### Recursive feature elimination ########

rfe.soja <- rfe(
  yield ~ .,
  data = a,
  sizes = c(1:25),
  rfeControl = rf_ctrl,
  number = 1000
)

predictors(rfe.soja)

plot(rfe.soja)

varImp(rfe.soja)

data.frame(Var = factor(rownames(varImp(rfe.soja))),
           Importance = varImp(rfe.soja)$Overall) %>%
  mutate(Var = forcats::fct_reorder(Var,Importance,.desc=F)) %>%
  ggplot(aes(Importance,Var))+
  geom_bar(stat='identity')+
  xlab("Importance (%)")+ylab("")


predictors(rfe.soja)

(varImp(rfe.soja))

plot(predict(rfe.soja,testing),testing$yield,xlim=c(-1.5,1),ylim=c(-1.5,1))
postResample(predict(rfe.soja,testing),testing$yield)

milho.knn <- train(
  yield ~ prec+prec_lag1+prec_lag2+prec_lag3+prec_lag4+spei+year+city_dummy,
  data = training,
  method='knn',
  length = 15,
  preProc =  c("center", "scale"),
  trControl = ctrl
)

milho.xgbLinear <- train(
  yield ~ prec+prec_lag1+prec_lag2+prec_lag3+prec_lag4+spei+year+city_dummy,
  data = training,
  method='xgbLinear',
  length = 5,
  preProc =  c("center", "scale"),
  trControl = ctrl
)

milho.rf <- train(
  yield ~ prec+prec_lag1+prec_lag2+prec_lag3+prec_lag4+spei+year+city_dummy,
  data = training,
  method='rf',
  length = 15,
  preProc =  c("center", "scale"),
  trControl = caret::trainControl(
    method="cv",
    number = 10
  )
)

milho.svmRadial <- train(
  yield ~ prec+prec_lag1+prec_lag2+prec_lag3+prec_lag4+spei+year+city_dummy,
  data = training,
  method='svmRadial',
  length = 150,
  preProc =  c("center", "scale"),
  trControl = ctrl
)


postResample(predict(milho.svmRadial,testing),testing$yield)



####### Soja ########




soja.rf2 <- train(
  yield ~ prec+prec_lag1+prec_lag2+prec_lag3+spei+year+rx5day+city_dummy,
  data = training,
  method='rf',
  length = 15,
  preProc =  c("center", "scale"),
  trControl = ctrl)

soja.ranger <- ranger(
  yield ~.,
  data = training,
  probability = TRUE
)

(importance(soja.ranger))

plot(soja.ranger$predictions,training$yield,ylim=c(-2,2),xlim=c(-2,2))

pred <- predict(soja.ranger,testing)
confusionMatrix(pred$predictions,testing$yield)

vload(file="modelo_soja.RData")
load(file="soybean_model.RData")
plot(model.soybean)

plot(varImp(model.soybean))

postResample(predict(model.soybean,testing),testing$yield)

plot(predict(soja.rf,testing),testing$yield)

soja.performance <- postResample(predict(soja.rf,testing),testing$yield)
n.soja <- nrow(testing)


head(varImp(soja.rf))
plot(rfe.soja$results$Rsquared)

plot(varImp(soja.rf))
rownames(varImp(soja.rf)$importance)
rownames(varImp(milho.rf)$importance)
c("Prec_Jan","Prec_Dec","Prec_Nov","Prec_Oct","SPEI","Year",
  "RX5day","City_dummy")

c("Prec_May","Prec_Apr","Prec_Mar","Prec_Feb","Prec_Jan","SPEI","Year","City_dummy")


ggarrange(data.frame(Var=c("Prec_Jan","Prec_Dec","Prec_Nov","Prec_Oct","SPEI","Year",
                           "RX5day","City_dummy"),
                     Importance=varImp(soja.rf)$importance$Overall) %>%
            ggplot(aes(Var,Importance))+
            geom_bar(stat="identity")+
            ylab("Importance (%)")+xlab("")+
            ggtitle("RF-Soybean")+
            theme(plot.title = element_text(hjust = 0.5)),
          data.frame(Var=c("Prec_May","Prec_Apr","Prec_Mar","Prec_Feb","Prec_Jan","SPEI","Year","City_dummy"),
                     Importance=varImp(milho.rf)$importance$Overall) %>%
            ggplot(aes(Var,Importance))+
            geom_bar(stat="identity")+
            ylab("Importance (%)")+xlab("")+
            ggtitle("RF-Maize")+
            theme(plot.title = element_text(hjust = 0.5)),
          labels = c("A","B"),ncol=1)


soja.data <- testing
soja.data$pred <- predict(soja.rf,testing)

soja.data %>%
  ggplot(aes(pred,yield))+
  geom_point(col='#D35400',size=0.8)+
  geom_smooth(method='lm',se=F,col='#16A085',size=0.75)+
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_vline(xintercept = 0,linetype="dotted")+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",col="#1B2631") +
  xlim(c(-1.5,1))+ylim(c(-1.5,1))+
  xlab("Predicted")+ylab("Observed")+
  annotate("text", x = -1., y = 1, label = paste("MAE: ",round(soja.performance[3],digits = 3)))+
  annotate("text", x = -1., y = 0.7, label = paste("R²: ",round(soja.performance[2],digits = 3)))+
  annotate("text", x = -1., y = 0.4, label = paste("n: ",round(n.soja,digits = 3)))+
  ggtitle("RF-Soybean (Test set)")+
  theme(plot.title = element_text(hjust = 0.5))



rfe.milho <- rfe(
  x = training[,-24],
  y = training[, 24],
  sizes = c(1:25),
  rfeControl = rf_ctrl)


rfe.soja <- rfe(
  x = training[,-24],
  y = training[, 24],
  sizes = c(1:25),
  rfeControl = rf_ctrl)



ggarrange(rfe.milho$results %>%
            ggplot(aes(Variables,Rsquared))+
            geom_point(col="cyan")+
            geom_point(data = rfe.milho$results %>% subset(Variables == 5),
                       aes(Variables,Rsquared),col='red')+
            ylim(c(0,0.9))+
            ggtitle("Variable Selection - Maize")+
            theme(plot.title = element_text(hjust = 0.5)),
          result_rfe1$results %>%
            ggplot(aes(Variables,Rsquared))+
            geom_point(col="cyan")+
            geom_point(data = result_rfe1$results %>% subset(Variables == 5),
                       aes(Variables,Rsquared),col='red')+
            ylim(c(0,0.9))+
            ggtitle("Variable Selection - Soybean")+
            theme(plot.title = element_text(hjust = 0.5)),
            labels = c("A","B"),ncol=2)

rfe.milho$results %>%
  ggplot(aes(Variables,Rsquared))+
  geom_point(col="cyan")+
  geom_point(data = rfe.milho$results %>% subset(Variables == 5),
             aes(Variables,Rsquared),col='red')+
  ggtitle("Variable Selection - Maize")+
  theme(plot.title = element_text(hjust = 0.5))



rfe.milho <- result_rfe1

rfe.milho
rfe.soja

save(rfe.milho,file="rfe.milho.RData")
save(rfe.soja,file="rfe.soja.RData")


postResample(predict(model,testing),(testing$yield))

postResample(predict(result_safs,testing),(testing$yield))

postResample(predict(model,testing),(testing$yield))

predictors(result_rfe1)

plot(predict(result_rfe1,testing),testing$yield,
     xlim=c(-2,1),ylim=c(-2,1))
abline(lm(testing$yield~predict(model,testing)))
lines(c(-10,10000),c(-10,10000))

boxplot(predict(model,testing),
        testing$yield)

a$pred <- predict(model,a)

a[c("year","pred","yield","city_dummy")] %>%
  pivot_longer(!c(year,city_dummy),names_to="model",values_to="vals") %>%
  group_by(city_dummy) %>%
  #summarise(cor = postResample(pred,yield)[2]) %>%
  #summarise(cor = cor(pred,yield)) %>%
  #ggplot(aes(cor))+geom_histogram()
  #ggplot(aes(pred,yield,group=city_dummy))+
  #geom_point()+
  #geom_smooth(method = 'lm',se=F)
  ggplot(aes(as.factor(year),vals,fill=model))+
  geom_boxplot()


stopCluster(Mycluster)



load(file="rfe.milho.RData")

plot(rfe.milho)

load(file="soybean_model.RData")


explainer <- shapr(training, soja.ranger,n_combinations = 100)
p <- mean(training$yield)
explanation <- explain(
  testing,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,
  n_samples = 100
)

print(explanation$dt)

plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))
