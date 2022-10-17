####################### IMPORTING LIBRARIES ######################
library(tidyverse)
library(ggplot2)
library(reshape2)
library(glmnet)
library(randomForest)
library(caret)
library(ggcorrplot)
########################### DATA IMPORT ##########################
BodyFat <- read.csv("D:/Aashna/STAT 628/Module 2/BodyFat_Clean.csv")
head(BodyFat)
df<-subset(BodyFat,select = -c(IDNO,DENSITY,ADIPOSITY,X))
head(df)
attach(df)
########################## BASIC MODELS ##########################
model1<-lm(BODYFAT~.,data=df)
summary(model1)
#model2<-lm(BODYFAT~.,data=BodyFat)
#summary(model2)
df2<-subset(BodyFat,select = -c(IDNO,DENSITY,HEIGHT,WEIGHT))
model3<-lm(BODYFAT~.,data=df2)
summary(model3)
model5<-lm(BODYFAT~AGE+NECK+ABDOMEN+FOREARM+WRIST,data=df) # highest accuracy
summary(model5)
model6<-lm(BODYFAT~WEIGHT+ABDOMEN,data=df)
summary(model6)
model7<-lm(BODYFAT~AGE+HEIGHT+WEIGHT,data=BodyFat)
summary(model7)
########################### CORRELATION CHECK ####################
corr<-round(cor(df),3)
head(corr)
melted_cor <- melt(corr)
head(melted_cor)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
#plot(df)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
######################## BODYFAT VS TOP 3 VARIABLES ###############
par(mfrow=c(2,2))

model_age<-lm(BODYFAT~AGE,data=df)
plot(df$AGE,df$BODYFAT,xlab = "Age",ylab="BodyFat")
#abline(model_age,col="red",lwd=3)
title("Bodyfat vs Age")

model_height<-lm(BODYFAT~HEIGHT,data=df)
plot(df$HEIGHT,df$BODYFAT,xlab = "Height",ylab="BodyFat")
#abline(model_height,col="blue",lwd=3)
title("Bodyfat vs Height")

model_ab<-lm(BODYFAT~ABDOMEN,data=df)
plot(df$ABDOMEN,df$BODYFAT,xlab = "Abdomen",ylab="BodyFat")
#abline(model_ab,col="green",lwd=3)
title("Bodyfat vs Abdomen")
##############################
par(mfrow=c(2,2))
par(mgp=c(1.8,.5,0), mar=c(3,3,1,1)) 
hist(BODYFAT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="Body Fat %")
hist(AGE,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Age",xlab="Age (yrs)")
hist(HEIGHT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Height ",xlab="Height (cm)")
hist(ABDOMEN,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram ofAbdomen",xlab="ABDOMEN (cm)")
###################### TOP MODEL COMPARISONS #####################
model_3<-lm(BODYFAT~AGE+ABDOMEN+HEIGHT,data=BodyFat) # 3rd
summary(model_3)
model_1<-lm(BODYFAT~ABDOMEN+HEIGHT,data=BodyFat) # 1st
summary(model_1)
plot(model)
model_2<-lm(BODYFAT~ABDOMEN,data=BodyFat) # 2nd
summary(model_2)
