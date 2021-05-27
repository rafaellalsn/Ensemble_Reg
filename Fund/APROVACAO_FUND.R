
#-------------APROVAÇÃO-------------
#-------------REPROVAÇÃO-------------
#-------------EVASÃO-------------
rm(list=ls(all=TRUE))

library(ggplot2)
library("caretEnsemble")
library("caret")
library("quantreg")  
library("mlbench")
library("pROC")
library("rpart")
library(randomForest)
library(MASS)
library(e1071)
library(ipred)
library(lqs)
library(readxl)
library(elasticnet)
setwd("D:\\Acadêmico 2016-2018\\Dissertação UPE\\dissertação de mestrado\\Mestrado\\RAFAELLA_\\FUND_TOTAL")
Dados <- read_excel("TX_APROVACAO_FUND.xlsx", sheet=1, col_names=TRUE)


TE = as.numeric(Dados$TA)
IRD = as.numeric(Dados$IRD)
TDI = as.numeric(Dados$TDI)
DSU = as.numeric(Dados$DSU)
ICG = as.numeric(Dados$ICG)
ATU = as.numeric(Dados$ATU)
IED1 = as.numeric(Dados$IED1)
IED2 = as.numeric(Dados$IED2)
IED3 = as.numeric(Dados$IED3)
IED4 = as.numeric(Dados$IED4)
IED5 = as.numeric(Dados$IED5)
IED6 = as.numeric(Dados$IED6)
AFD1 = as.numeric(Dados$AFD1)
AFD2 = as.numeric(Dados$AFD2)
AFD3 = as.numeric(Dados$AFD3)
AFD4 = as.numeric(Dados$AFD4)
AFD5 = as.numeric(Dados$AFD5)

#X = cbind(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,AFD1,AFD2,AFD3,AFD4,AFD5,AFD6,TA,TR,TE)
#sem AFGD1
DadosAprovacao = data.frame(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,IED6,AFD1,AFD2,AFD3,AFD4,AFD5,TE)
#DadosReprovacao = data.frame(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,IED6,AFD1,AFD2,AFD3,AFD4,AFD5,TA,TR,TE)
#DadosEvasao = data.frame(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,IED6,AFD1,AFD2,AFD3,AFD4,AFD5,TA,TR,TE)
TA=TE
DadosAprovacao1 = data.frame(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,IED6,AFD1,AFD2,AFD3,AFD4,AFD5,TA)

#plot(DadosAprovacao$TDI,DadosAprovacao$TE)
#--------------------------REMOVER LINHAS EM BRANCO-------------------#

#linhas <- c(4180,4181,4182,4183,4184,4185,4186,4187,4188)
#DadosAprovacao2 <- DadosAprovacao1[-linhas,]
sum(is.na(DadosAprovacao))


#--------------------------NORMALIZACAO DOS DADOS---------------------#

for(i in 1:(dim(DadosAprovacao)[2])){
  min <- min(DadosAprovacao[,i])
  max <- max(DadosAprovacao[,i])
  DadosAprovacao[,i] <- (DadosAprovacao[,i]-min)/(max-min)
}

for(i in 1:(dim(DadosAprovacao1)[2])){
  min <- min(DadosAprovacao1[,i])
  max <- max(DadosAprovacao1[,i])
  DadosAprovacao1[,i] <- (DadosAprovacao1[,i]-min)/(max-min)
}


#---------------------------MATRIZ DE CORRELAÇÃO----------------------#

#DadosAprovacao=unique(DadosAprovacao)

library(corrplot)
corrplot(cor(DadosAprovacao1), method="square", tl.col="black", tl.srt=90, tl.cex = 0.7)
cor(DadosAprovacao1)
#require(randomForest)
#Var_Importance = randomForest(TE~IRD+TDI+ICG+DSU+ATU+IED1+IED2+IED3+IED4+IED5+IED6+AFD1+AFD2+AFD3+AFD4+AFD5, data=DadosAprovacao, ntree=1000, keep.forest=FALSE, importance=TRUE)
#varImpPlot(Var_Importance,1)
#-------------------------EXCLUIR MULTICOLINEARIDADE------------------#


#---------------------------HISTOGRAMA--------------------------------#

#require(reshape2)
#require(ggplot2)
#df <- melt(X_Imp)
#ggplot(df,aes(x = value)) + 
#  facet_wrap(~variable, scales = "free_x") +
#  geom_histogram()

#----------------------------PARTICAO DOS DADOS-------------------------------#


Todos_RMSE_lm = NULL
Todos_MAE_lm = NULL
Todos_MRE_lm = NULL
Todos_RMSE_rlm = NULL
Todos_MAE_rlm = NULL
Todos_MRE_rlm = NULL
Todos_RMSE_rq = NULL
Todos_MAE_rq = NULL
Todos_MRE_rq = NULL
Todos_RMSE_ensemble = NULL
Todos_MAE_ensemble = NULL
Todos_MRE_ensemble = NULL
Todos_RMSE_svr = NULL
Todos_MAE_svr = NULL
Todos_MRE_svr = NULL
Todos_MAE_r = NULL
Todos_MRE_r = NULL
Todos_RMSE_r = NULL
Todos_MAE_rs = NULL
Todos_MRE_rs = NULL
Todos_RMSE_rs = NULL
#plot(base)


n <- 30
for (ii in 1:n) {
  cat(paste0(round(ii / n * 100), '% completed'))
  Sys.sleep(.05)
  if (ii == n) cat(': Done')
  else cat('\014')
  
  trainIndex <- createDataPartition(DadosAprovacao$TE, p = .75, list = FALSE)
  
  train <- DadosAprovacao[trainIndex,]
  test <- DadosAprovacao[-trainIndex,]
  
#ridge
  #ridge <- ridge::linearRidge(TE ~ TDI+ICG+ATU+AFD1,data=train)
  #prediction_r <- predict(ridge,newdata = test)
  #r_prediction <- data.frame(pred = prediction_r, obs = test$TE)
  #MAE_r <- mean(abs(r_prediction$pred - r_prediction$obs))
  #Todos_MAE_r[ii] <- MAE_r
  #MRE_r <- mean(abs(r_prediction$pred - r_prediction$obs)/r_prediction$obs)
  #Todos_MRE_r[ii] <- MRE_r
  #RMSE_r = sqrt(mean((r_prediction$pred - r_prediction$obs)^2))
  #Todos_RMSE_r[ii] <- RMSE_r
  
  robust <- rlm(TE ~ TDI+ATU,data=train,maxit=100)
  
  #TDI+ATU+IED3
  
  linear <- lm(TE ~ TDI+ATU,data=train)
  
  #gradlinear <- glm(TA ~ IRD+TDI+ICG+DSU+ATU+IED1+IED2+IED3+IED4+IED5+IED6+AFD1+AFD2+AFD3+AFD4+AFD5,data=train,family = gaussian)
  
  quantil <- rq(TE ~ TDI+ATU,data=train,tau=0.5)
  
  support <- svm(TE ~ TDI+ATU,data=train, kernel='radial')#,kernel='linear'
  
  resistant <- ltsreg(TE ~ TDI+ATU,data=train,method="lqs")
  
  prediction_robust <- predict(robust,newdata = test)
  robust_prediction <- data.frame(pred = prediction_robust, obs = test$TE)
  
  prediction_resistant <- predict(resistant,newdata = test)
  resistant_prediction <- data.frame(pred = prediction_resistant, obs = test$TE)
  
  prediction_linear <- predict(linear,newdata = test)
  linear_prediction <- data.frame(pred = prediction_linear, obs = test$TE)
  
  #prediction_gradlinear <- predict(gradlinear,newdata = test)
  #gradlinear_prediction <- data.frame(pred = prediction_gradlinear, obs = test$TA)
  
  prediction_quantil <- predict(quantil,newdata = test)
  quantil_prediction <- data.frame(pred = prediction_quantil, obs = test$TE)
  
  prediction_support <- predict(support, test)
  support_prediction <- data.frame(pred = prediction_support, obs = test$TE)
  
  MAE_rs <- mean(abs(resistant_prediction$pred - resistant_prediction$obs))
  Todos_MAE_rs[ii] <- MAE_rs
  MRE_rs <- mean(abs(resistant_prediction$pred - resistant_prediction$obs)/resistant_prediction$obs)
  Todos_MRE_rs[ii] <- MRE_rs
  RMSE_rs = sqrt(mean((resistant_prediction$pred - resistant_prediction$obs)^2))
  Todos_RMSE_rs[ii] <- RMSE_rs
  
  MAE_lm <- mean(abs(linear_prediction$pred - linear_prediction$obs))
  Todos_MAE_lm[ii] <- MAE_lm
  MRE_lm <- mean(abs(linear_prediction$pred - linear_prediction$obs)/linear_prediction$obs)
  Todos_MRE_lm[ii] <- MRE_lm
  RMSE_lm = sqrt(mean((linear_prediction$pred - linear_prediction$obs)^2))
  Todos_RMSE_lm[ii] <- RMSE_lm
  
  #MAE_glm <- mean(abs(gradlinear_prediction$pred - gradlinear_prediction$obs))
  #Todos_MAE_glm[ii] <- MAE_glm
  #MRE_glm <- mean(abs(gradlinear_prediction$pred - gradlinear_prediction$obs)/gradlinear_prediction$obs)
  #Todos_MRE_glm[ii] <- MRE_glm
  #RMSE_glm = sqrt(mean((gradlinear_prediction$pred - gradlinear_prediction$obs)^2))
  #Todos_RMSE_glm[ii] <- RMSE_glm
  
  MAE_rlm <- mean(abs(robust_prediction$pred - robust_prediction$obs))
  Todos_MAE_rlm[ii] <- MAE_rlm
  MRE_rlm <- mean(abs(robust_prediction$pred - robust_prediction$obs)/robust_prediction$obs)
  Todos_MRE_rlm[ii] <- MRE_rlm
  RMSE_rlm = sqrt(mean((robust_prediction$pred - robust_prediction$obs)^2))
  Todos_RMSE_rlm[ii] <- RMSE_rlm
  
  MAE_rq <- mean(abs(quantil_prediction$pred - quantil_prediction$obs))
  Todos_MAE_rq[ii] <- MAE_rq
  MRE_rq <- mean(abs(quantil_prediction$pred - quantil_prediction$obs)/quantil_prediction$obs)
  Todos_MRE_rq[ii] <- MRE_rq
  RMSE_rq = sqrt(mean((quantil_prediction$pred - quantil_prediction$obs)^2))
  Todos_RMSE_rq[ii] <- RMSE_rq
  
  MAE_svr <- mean(abs(support_prediction$pred - support_prediction$obs))
  Todos_MAE_svr[ii] <- MAE_svr
  MRE_svr <- mean(abs(support_prediction$pred - support_prediction$obs)/support_prediction$obs)
  Todos_MRE_svr[ii] <- MRE_svr
  RMSE_svr = sqrt(mean((support_prediction$pred - support_prediction$obs)^2))
  Todos_RMSE_svr[ii] <- RMSE_svr
  
  train$LM<-linear$fitted.values
  #train$GLM<-gradlinear$fitted.values
  train$RQ<-quantil$fitted.values
  train$RLM<-robust$fitted.values
  train$SVR<-support$fitted
  train$RS<-resistant$fitted.values
  #train$RA<-ridge$y
  
  #test$GLM<-gradlinear_prediction$pred
  test$LM<-linear_prediction$pred
  test$RLM<-robust_prediction$pred
  test$RQ<-quantil_prediction$pred
  test$SVR<-support_prediction$pred
  test$RS<-resistant_prediction$pred
  #test$RA<-r_prediction$pred
  
  predictors_top<-c('LM','RQ','RLM','RS') 
  model_ensemble <- svm(TE ~ RQ+LM+RLM+RS, data=test)
  prediction_ensemble<-predict(model_ensemble,newdata = test)
  en_prediction <- data.frame(pred = prediction_ensemble, obs = test$TE)
  
  MAE_ensemble <- mean(abs(en_prediction$pred - en_prediction$obs))
  Todos_MAE_ensemble[ii] <- MAE_ensemble
  MRE_ensemble <- mean(abs(en_prediction$pred - en_prediction$obs)/en_prediction$obs+1)
  Todos_MRE_ensemble[ii] <- MRE_ensemble
  RMSE_ensemble = sqrt(mean((en_prediction$pred - en_prediction$obs)^2))
  Todos_RMSE_ensemble[ii] <- RMSE_ensemble
  
  Dados_Erro = data.frame(MAE_ensemble,MAE_lm,MAE_rlm,MAE_rq,MAE_rs,MAE_svr)
  #Dados_Tempo = data.frame(Todos_tempo_en,Todos_tempo_lm,Todos_tempo_rlm,Todos_tempo_rq,Todos_tempo_rs,Todos_tempo_svr)
  
  write.table(Dados_Erro, file="resultados_erro_AF.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)
  #write.table(Dados_Tempo, file="resultados_tempo_AF.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)
  
}

boxplot(las = 1, names=c('RL','RS','RQ','RLR','SVR','ENS') ,
        Todos_MAE_lm,Todos_MAE_rs,Todos_MAE_rq,Todos_MAE_rlm,Todos_MAE_svr,Todos_MAE_ensemble)

#boxplot(las = 1, names=c('Robusta','Linear'),Todos_MAE_rlm,Todos_MAE_lm)


#Real = robust_prediction$obs
#Predito = robust_prediction$pred

#par(mfrow=c(1,1))
#plot(Predito,Real,col='black',main='Real vs Predito Regressão Linear',pch=18,cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='Regressão ',pch=18,col='black', bty='n')


mae_lm=mean(Todos_MAE_lm)
mae_rlm=mean(Todos_MAE_rlm)
mae_rs=mean(Todos_MAE_rs)
mae_rq=mean(Todos_MAE_rq)
mae_svr=mean(Todos_MAE_svr)
#mae_r=mean(Todos_MAE_r)
mae_en=mean(Todos_MAE_ensemble)

mae_rlm1=sd(Todos_MAE_rlm)
mae_rs1=sd(Todos_MAE_rs)
mae_rq1=sd(Todos_MAE_rq)
mae_lm1=sd(Todos_MAE_lm)
mae_svr1=sd(Todos_MAE_svr)
mae_en1=sd(Todos_MAE_ensemble)

MAE= data.frame(mae_rlm,mae_rs,mae_rq,mae_lm,mae_svr,mae_en)
MAE1= data.frame(mae_rlm1,mae_rs1,mae_rq1,mae_lm1,mae_svr1,mae_en1)
MAE

mre_rlm=mean(Todos_MRE_rlm)
mre_rq=mean(Todos_MRE_rq)
mre_lm=mean(Todos_MRE_lm)
mre_rs=mean(Todos_MRE_rs)
mre_svr=mean(Todos_MRE_svr)
mre_en=mean(Todos_MRE_ensemble)

MRE= data.frame(mre_rlm,mre_rs,mre_rq,mre_lm,mre_svr,mre_en)

rmse_rlm=mean(Todos_RMSE_rlm)
rmse_rq=mean(Todos_RMSE_rq)
rmse_lm=mean(Todos_RMSE_lm)
rmse_svr=mean(Todos_RMSE_svr)
rmse_rs=mean(Todos_RMSE_rs)
rmse_en=mean(Todos_RMSE_ensemble)

RMSE= data.frame(rmse_rlm,rmse_rs,rmse_rq,rmse_lm,rmse_svr,rmse_en)

rmse_rlm1=sd(Todos_RMSE_rlm)
rmse_rq1=sd(Todos_RMSE_rq)
rmse_lm1=sd(Todos_RMSE_lm)
rmse_svr1=sd(Todos_RMSE_svr)
rmse_rs1=sd(Todos_RMSE_rs)
rmse_en1=sd(Todos_RMSE_ensemble)

RMSE1= data.frame(rmse_rlm1,rmse_rs1,rmse_rq1,rmse_lm1,rmse_svr1,rmse_en1)

RMSE
MAE
MAE1

library(ggplot2)
myplot=ggplot() +
  geom_point(aes(x=train$TDI , y=train$TE), 
             colour="black",size=1,shape=1)+
  geom_point(aes(x = test$TDI, y = prediction_robust, color = 'RLR'),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_robust, color = 'RLR')) +
  geom_point(aes(x = test$TDI, y = prediction_linear, color = 'RL'),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_linear, color = 'RL')) +
  geom_point(aes(x = test$TDI, y = prediction_quantil, color = 'RQ'),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_quantil, color = 'RQ')) +
  geom_point(aes(x = test$TDI, y = prediction_resistant, color = 'RS'),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_resistant, color = 'RS')) +
  geom_point(aes(x = test$TDI, y = prediction_support, color = 'SVR'),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_support, color = 'SVR')) +
  geom_point(aes(x = test$TDI, y = prediction_ensemble,color="ENS"),size=1) +
  geom_line(aes(x = test$TDI, y = prediction_ensemble,color = "ENS")) +
  scale_color_manual(name = "Modelo", # legend name
                     values = c("ENS" = "black",  # map regression line colors
                                "RLR" = "darkorange",
                                "RL" = "blue",
                                "RQ" = "green",
                                "RS" = "purple",
                                'SVR' = "red"))+
  xlab('x = TDI')+ylab('y = TR')
myplot+theme_bw()



TesteNormalidade1 <- shapiro.test(Todos_MAE_rq)
TesteNormalidade2 <- shapiro.test(Todos_MAE_lm)
TesteNormalidade3 <- shapiro.test(Todos_MAE_rs)
TesteNormalidade4 <- shapiro.test(Todos_MAE_rlm)
TesteNormalidade5 <- shapiro.test(Todos_MAE_svr)
TesteNormalidade6 <- shapiro.test(Todos_MAE_ensemble)
TesteNormalidade1
#hist(Todos_MAE_rlm)

t.test(Todos_MAE_ensemble, Todos_MAE_rq, alternative = "less", paired=TRUE)
t.test(Todos_MAE_ensemble, Todos_MAE_lm, alternative = "less", paired=TRUE)
t.test(Todos_MAE_ensemble, Todos_MAE_rs, alternative = "less", paired=TRUE)
t.test(Todos_MAE_ensemble, Todos_MAE_rlm, alternative = "less", paired=TRUE)
t.test(Todos_MAE_ensemble, Todos_MAE_svr, alternative = "less", paired=TRUE)


wilcox.test(Todos_MAE_ensemble, Todos_MAE_rq, alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_lm,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_rs,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_rlm,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_svr,alternative = "less", paired=TRUE)


