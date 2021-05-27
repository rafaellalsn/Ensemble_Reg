
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
#library(lqs)
library(readxl)

setwd("C:\\Users\\Rafaella Souza\\Desktop\\Mestrado\\RAFAELLA_\\MEDIO_TOTAL")
Dados <- read_excel("TX_REND_MEDIO_EVASAO.xlsx", sheet=1, col_names=TRUE)

TE = as.numeric(Dados$TE)
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

#plot(DadosAprovacao$TDI,DadosAprovacao$TE)
#--------------------------REMOVER LINHAS EM BRANCO-------------------#

#linhas <- c(4180,4181,4182,4183,4184,4185,4186,4187,4188)
#DadosAprovacao2 <- DadosAprovacao1[-linhas,]
#sum(is.na(DadosAprovacao))


#--------------------------NORMALIZACAO DOS DADOS---------------------#

for(i in 1:(dim(DadosAprovacao)[2])){
  min <- min(DadosAprovacao[,i])
  max <- max(DadosAprovacao[,i])
  DadosAprovacao[,i] <- (DadosAprovacao[,i]-min)/(max-min)
}


#---------------------------MATRIZ DE CORRELAÇÃO----------------------#

#DadosAprovacao=unique(DadosAprovacao)

#library(corrplot)
#corrplot(cor(DadosAprovacao), method="number", tl.col="black", tl.srt=90, tl.cex = 0.7)
#cor(DadosAprovacao)
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
Todos_tempo_en = NULL
Todos_tempo_rs = NULL
Todos_tempo_lm = NULL
Todos_tempo_rlm = NULL
Todos_tempo_rq = NULL
Todos_tempo_svr = NULL
#plot(base)
file.create("resultados_erro.csv")
file.create("resultados_tempo.csv")

n <- 30
for (ii in 1:n) {
  ptm_en <- Sys.time()
  
  trainIndex <- createDataPartition(DadosAprovacao$TE, p = .75, list = FALSE)
  
  train <- DadosAprovacao[trainIndex,]
  test <- DadosAprovacao[-trainIndex,]
  #DSU+ATU+AFD1
  #ATU
  ptm_rlm <- Sys.time()
  robust <- rlm(TE ~ DSU+ATU+AFD1,data=train,maxit=100)
  prediction_robust <- predict(robust,newdata = test)
  robust_prediction <- data.frame(pred = prediction_robust, obs = test$TE)
  MAE_rlm <- mean(abs(robust_prediction$pred - robust_prediction$obs))
  Todos_MAE_rlm[ii] <- MAE_rlm
  Todos_tempo_rlm[ii]=Sys.time() - ptm_rlm
  
  ptm_lm <- Sys.time()
  linear <- lm(TE~ DSU+ATU+AFD1,data=train)
  prediction_linear <- predict(linear,newdata = test)
  linear_prediction <- data.frame(pred = prediction_linear, obs = test$TE)
  MAE_lm <- mean(abs(linear_prediction$pred - linear_prediction$obs))
  Todos_MAE_lm[ii] <- MAE_lm
  Todos_tempo_lm[ii]=Sys.time() - ptm_lm
  
  ptm_rq <- Sys.time()
  quantil <- rq(TE ~ DSU+ATU+AFD1,data=train,tau=0.5)
  prediction_quantil <- predict(quantil,newdata = test)
  quantil_prediction <- data.frame(pred = prediction_quantil, obs = test$TE)
  MAE_rq <- mean(abs(quantil_prediction$pred - quantil_prediction$obs))
  Todos_MAE_rq[ii] <- MAE_rq
  Todos_tempo_rq[ii]=Sys.time() - ptm_rq
  
  ptm_svr <- Sys.time()
  support <- svm(TE ~ DSU+ATU+AFD1,data=train)#,kernel='linear'
  prediction_support <- predict(support, test)
  support_prediction <- data.frame(pred = prediction_support, obs = test$TE)
  MAE_svr <- mean(abs(support_prediction$pred - support_prediction$obs))
  Todos_MAE_svr[ii] <- MAE_svr
  Todos_tempo_svr[ii]=Sys.time() - ptm_svr
  
  ptm_rs <- Sys.time()
  resistant <- ltsreg(TE ~ DSU+ATU+AFD1,data=train,method="lqs")
  prediction_resistant <- predict(resistant,newdata = test)
  resistant_prediction <- data.frame(pred = prediction_resistant, obs = test$TE)
  MAE_rs <- mean(abs(resistant_prediction$pred - resistant_prediction$obs))
  Todos_MAE_rs[ii] <- MAE_rs
  Todos_tempo_rs[ii]=Sys.time() - ptm_rs
  
  train$LM<-linear$fitted.values
  train$RQ<-quantil$fitted.values
  train$RLM<-robust$fitted.values
  train$SVR<-support$fitted
  train$RS<-resistant$fitted.values
  
  test$LM<-linear_prediction$pred
  test$RLM<-robust_prediction$pred
  test$RQ<-quantil_prediction$pred
  test$SVR<-support_prediction$pred
  test$RS<-resistant_prediction$pred
  
  
  predictors_top<-c('LM','RQ','RLM','RS') 
  model_ensemble <- svm(TE ~ RQ+LM+RLM+RS, data=test)
  prediction_ensemble<-predict(model_ensemble,newdata = test)
  en_prediction <- data.frame(pred = prediction_ensemble, obs = test$TE)
  
  MAE_ensemble <- mean(abs(en_prediction$pred - en_prediction$obs))
  Todos_MAE_ensemble[ii] <- MAE_ensemble
  
  Todos_tempo_en[ii]=Sys.time() - ptm_en
  
  Dados_Erro = data.frame(MAE_ensemble,MAE_lm,MAE_rlm,MAE_rq,MAE_rs,MAE_svr)
  Dados_Tempo = data.frame(Todos_tempo_en,Todos_tempo_lm,Todos_tempo_rlm,Todos_tempo_rq,Todos_tempo_rs,Todos_tempo_svr)
  
  write.table(Dados_Erro, file="resultados_erro.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)
  write.table(Dados_Tempo, file="resultados_tempo.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)
  
}



file.create("resultados_pred_rq.csv")
write.table(quantil_prediction, file="resultados_pred_rq.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_pred_rlm.csv")
write.table(robust_prediction, file="resultados_pred_rlm.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_pred_lm.csv")
write.table(linear_prediction, file="resultados_pred_lm.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_pred_rs.csv")
write.table(resistant_prediction, file="resultados_pred_rs.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_pred_svr.csv")
write.table(support_prediction, file="resultados_pred_svr.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_pred_ensemble.csv")
write.table(en_prediction, file="resultados_pred_ensemble.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

#-------salvar erro ---------------------#

file.create("resultados_mae_rq.csv")
write.table(Todos_MAE_rq, file="resultados_mae_rq.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_mae_rlm.csv")
write.table(Todos_MAE_rlm, file="resultados_mae_rlm.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_mae_lm.csv")
write.table(Todos_MAE_lm, file="resultados_mae_lm.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_mae_rs.csv")
write.table(Todos_MAE_rs, file="resultados_mae_rs.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_mae_svr.csv")
write.table(Todos_MAE_svr, file="resultados_mae_svr.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)

file.create("resultados_mae_ensemble.csv")
write.table(Todos_MAE_ensemble, file="resultados_mae_ensemble.csv",row.names=FALSE, col.names=FALSE, sep=";",append = TRUE)


#png("grafico_boxplo_MAE_EVASAO_Medio.png", width = 1200, height = 900, res = 196)
boxplot(las = 1, names=c('RL','RS','RQ','RLR','SVR','ENS') ,
        Todos_MAE_lm,Todos_MAE_rs,Todos_MAE_rq,Todos_MAE_rlm,Todos_MAE_svr,Todos_MAE_ensemble)
#dev.off()

mae_rlm=mean(Todos_MAE_rlm)
mae_rs=mean(Todos_MAE_rs)
mae_rq=mean(Todos_MAE_rq)
mae_lm=mean(Todos_MAE_lm)
mae_svr=mean(Todos_MAE_svr)
mae_en=mean(Todos_MAE_ensemble)

mae_rlm1=sd(Todos_MAE_rlm)
mae_rs1=sd(Todos_MAE_rs)
mae_rq1=sd(Todos_MAE_rq)
mae_lm1=sd(Todos_MAE_lm)
mae_svr1=sd(Todos_MAE_svr)
mae_en1=sd(Todos_MAE_ensemble)

time_rlm=mean(Todos_tempo_rlm)
time_rs=mean(Todos_tempo_rs)
time_rq=mean(Todos_tempo_rq)
time_lm=mean(Todos_tempo_lm)
time_svr=mean(Todos_tempo_svr)
time_en=mean(Todos_tempo_en)

MAE= data.frame(mae_rlm,mae_rs,mae_rq,mae_lm,mae_svr,mae_en)
MAE1= data.frame(mae_rlm1,mae_rs1,mae_rq1,mae_lm1,mae_svr1,mae_en1)
time= data.frame(time_rlm,time_rs,time_rq,time_lm,time_svr,time_en)

MAE
MAE1
#time

library(ggplot2)
myplot=ggplot() +
  geom_point(aes(x=DadosAprovacao$ATU , y=DadosAprovacao$TE), 
             colour="black",size=1,shape=1)+
  geom_point(aes(x = test$ATU, y = prediction_robust, color = 'RLR'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_robust, color = 'RLR')) +
  geom_point(aes(x = test$ATU, y = prediction_linear, color = 'RL'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_linear, color = 'RL')) +
  geom_point(aes(x = test$ATU, y = prediction_quantil, color = 'RQ'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_quantil, color = 'RQ')) +
  geom_point(aes(x = test$ATU, y = prediction_resistant, color = 'RS'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_resistant, color = 'RS')) +
  geom_point(aes(x = test$ATU, y = prediction_support, color = 'SVR'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_support, color = 'SVR')) +
  geom_point(aes(x = test$ATU, y = prediction_ensemble,color='ENS'),size=1) +
  geom_line(aes(x = test$ATU, y = prediction_ensemble,color = "ENS")) +
  scale_color_manual(name = "Modelo",             # legend name
                     values = c("ENS" = "black",  # map regression line colors
                                "RLR" = "darkorange",
                                "RL" = "blue",
                                "RQ" = "green",
                                "RS" = "purple",
                                'SVR' = "red"))+
  xlab('x = ATU') +ylab('y = TE')
myplot+theme_bw()


TesteNormalidade1 <- shapiro.test(Todos_MAE_rq)
TesteNormalidade2 <- shapiro.test(Todos_MAE_lm)
TesteNormalidade3 <- shapiro.test(Todos_MAE_rs)
TesteNormalidade4 <- shapiro.test(Todos_MAE_rlm)
TesteNormalidade5 <- shapiro.test(Todos_MAE_svr)
TesteNormalidade6 <- shapiro.test(Todos_MAE_ensemble)
#TesteNormalidade1
#hist(Todos_MAE_rlm)

#t.test(Todos_MAE_ensemble, Todos_MAE_rq, alternative = "less", paired=TRUE)
#t.test(Todos_MAE_ensemble, Todos_MAE_lm, alternative = "less", paired=TRUE)
#t.test(Todos_MAE_ensemble, Todos_MAE_rs, alternative = "less", paired=TRUE)
#t.test(Todos_MAE_ensemble, Todos_MAE_rlm, alternative = "less", paired=TRUE)
#t.test(Todos_MAE_ensemble, Todos_MAE_svr, alternative = "less", paired=TRUE)


wilcox.test(Todos_MAE_ensemble, Todos_MAE_rq, alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_lm,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_rs,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_rlm,alternative = "less", paired=TRUE)
wilcox.test(Todos_MAE_ensemble, Todos_MAE_svr,alternative = "less", paired=TRUE)


