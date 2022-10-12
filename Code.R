####################### IMPORTING LIBRARIES ######################
library(tidyverse)
library(ggplot2)
library(reshape2)
library(glmnet)
########################### DATA IMPORT ##########################
BodyFat <- read.csv("D:/Aashna/STAT 628/Module 2/BodyFat_Clean.csv")
head(BodyFat)
df<-subset(BodyFat,select = -c(IDNO,DENSITY,ADIPOSITY))
head(df)
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
plot(df)
##################### CORRELATION AFTER SCALING #################
df3 <- df %>% mutate_all(~(scale(.) %>% as.vector))
head(df3)
corr2<-round(cor(df3),3)
head(corr2)
melted_cor2 <- melt(corr2)
head(melted_cor2)
ggplot(data = melted_cor2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
########################## BASIC MODELS ##########################
model1<-lm(BODYFAT~.,data=df)
summary(model1)
#model2<-lm(BODYFAT~.,data=BodyFat)
#summary(model2)
df2<-subset(BodyFat,select = -c(IDNO,DENSITY,HEIGHT,WEIGHT))
model3<-lm(BODYFAT~.,data=df2)
summary(model3)
model4<-lm(BODYFAT~AGE+WEIGHT+HEIGHT,data=df)
summary(model4)
model5<-lm(BODYFAT~AGE+NECK+ABDOMEN+HIP+FOREARM+WRIST,data=df)
summary(model5)
model4<-lm(BODYFAT~WEIGHT+HEIGHT,data=df)
summary(model4)
############################ LASSO & RIDGE #######################
X<-as.matrix(df[,-1])
Y<-as.matrix(df[1])
set.seed(100)
cv.lasso <- cv.glmnet(X, Y, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
cv.lasso <- cv.glmnet(X, Y)
plot(cv.lasso)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)
df_coef[df_coef[, 1] != 0, ]
cv.ridge<-cv.glmnet(X,Y,aplha=0)
df_coef2 <- round(as.matrix(coef(cv.ridge, s=cv.ridge$lambda.min)), 2)
df_coef2[df_coef2[, 1] != 0, ]
###################### TOP MODEL COMPARISONS #####################
model<-lm(BODYFAT~AGE+ABDOMEN+HEIGHT,data=BodyFat) # 3rd
summary(model)
model<-lm(BODYFAT~ABDOMEN+HEIGHT,data=BodyFat) # 1st
summary(model)
plot(model)
model<-lm(BODYFAT~ABDOMEN,data=BodyFat) # 2nd
summary(model)
