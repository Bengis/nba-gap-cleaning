## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load_libraries, include=FALSE---------------------------------------
library(knitr)
library(lubridate)
library(VIM)
library(stringr)
library(psych)
library(pROC)
library(dplyr)

## ---- echo=TRUE----------------------------------------------------------
# read data
nba_org <- read.csv("../data/nba-stats_in.csv")
nba_org["sex"] <- NA
nba_org$sex<-"0"
wnba_org <- read.csv("../data/wnba-stats_in.csv")
wnba_org["sex"] <- NA
wnba_org$sex<-"1"
t_nba <- rbind(nba_org, wnba_org)
n.var <- names(t_nba)

## ---- echo=TRUE----------------------------------------------------------
t_nba<-t_nba[,-10:-14]

## ---- echo=TRUE----------------------------------------------------------
# read data
res <- sapply(t_nba,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

## ---- echo=TRUE----------------------------------------------------------
t_nba[2:8] <- lapply(t_nba[2:8], as.numeric)
res <- sapply(t_nba,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

## ---- echo=TRUE----------------------------------------------------------
t_nba_reduced<-t_nba[,-4:-10]
t_nba_reduced<-t_nba_reduced[,-1:-1]
pairs(t_nba_reduced)

## ---- echo=TRUE----------------------------------------------------------
t_nba_reduced<-t_nba[,-5:-10]
t_nba_reduced<-t_nba_reduced[,-1:-2]
pairs(t_nba_reduced)

## ---- echo=TRUE----------------------------------------------------------
t_nba<-t_nba[which(t_nba$games!="0"),]

## ---- echo=TRUE----------------------------------------------------------
games_m=82
games_w=34
for(i in 2:8) {
    t_nba[which(t_nba$sex=="0"),i] <- t_nba[which(t_nba$sex=="0"),i]/games_m
    t_nba[which(t_nba$sex=="1"),i] <- t_nba[which(t_nba$sex=="1"),i]/games_w
}

## ---- echo=TRUE----------------------------------------------------------
t_nba<-t_nba[,-2:-2]

## ---- echo=TRUE----------------------------------------------------------
t_nba<-t_nba[which(t_nba$minutes!="0"),]

## ---- echo=TRUE----------------------------------------------------------
remove_outliers <- function(x, limit = 3) {
    mn <- mean(x, na.rm = T)
    out <- limit * sd(x, na.rm = T)
    x < (mn - out) | x > (mn + out)
}
t_nba<-t_nba[remove_outliers(t_nba$minutes,3)==FALSE,]

## ----boxplot,eval=TRUE,echo=TRUE-----------------------------------------
par(mfrow=c(2,2))
for(i in 1:ncol(t_nba)) {
  if (is.numeric(t_nba[,i])){
    boxplot(t_nba[,i], main = colnames(t_nba)[i], width = 100)
  }
}
par(mfrow=c(1,1))

## ---- echo=TRUE----------------------------------------------------------
filas_bro<-nrow(t_nba)
t_nba<-t_nba[remove_outliers(t_nba$rebds.,3)==FALSE,]
t_nba<-t_nba[remove_outliers(t_nba$assists,3)==FALSE,]
t_nba<-t_nba[remove_outliers(t_nba$blocks,3)==FALSE,]
filas_aro<-nrow(t_nba)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
par(mfrow=c(2,2))
for(i in 1:ncol(t_nba)) {
  if (is.numeric(t_nba[,i])){
    qqnorm(t_nba[,i],main = paste("Normal Q-Q Plot for ",colnames(t_nba)[i]))
    qqline(t_nba[,i],col="red")
    hist(t_nba[,i], 
      main=paste("Histogram for ", colnames(t_nba)[i]), 
      xlab=colnames(t_nba)[i], freq = FALSE)
  }
}

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$minutes)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$points)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$rebds.)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$assists)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$steals)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$blocks)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
shapiro.test(t_nba$salary)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
t_nba_glm<-t_nba[which(t_nba$sex=="0"),]
ntrain <- nrow(t_nba_glm)*0.8
ntest <- nrow(t_nba_glm)*0.2
set.seed(1)
index_train<-sample(1:nrow(t_nba_glm),size = ntrain)
train<-t_nba_glm[index_train,]
test<-t_nba_glm[-index_train,]
modelo<-lm(formula = salary ~ points, data=train)
summary(modelo)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
prob_sl<-predict(modelo, test, type="response")
mc_sl<-data.frame(
  real=test$salary,
  predicted= prob_sl,
  dif=ifelse(test$salary>prob_sl, -prob_sl*100/test$salary,prob_sl*100/test$salary)
  )
colnames(mc_sl)<-c("Real","Predecido","Dif%")
kable(mc_sl)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
test<-t_nba[which(t_nba$sex=="1"),]
prob_sl<-predict(modelo, test, type="response")
mc_sl<-data.frame(
  real=test$salary,
  predicted= prob_sl,
  dif=ifelse(test$salary>prob_sl, -prob_sl*100/test$salary,prob_sl*100/test$salary)
  )
colnames(mc_sl)<-c("Real","Predecido","Dif%")
kable(mc_sl)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
options(scipen=5)
nba_salary=sum(as.numeric(t_nba$salary[which(t_nba$sex==0)])/length(which(t_nba$sex==0)))
wnba_salary=sum(as.numeric(t_nba$salary[which(t_nba$sex==1)])/length(which(t_nba$sex==1)))
counts <- c(nba_salary, wnba_salary)
barplot(counts, names=c("NBA", "WNBA"), main="Media de salarios por jugador de cada liga", 
  	xlab="Liga")

## ----eval=TRUE,echo=TRUE-------------------------------------------------
write.csv(t_nba, file = "../data/nba_out.csv")

