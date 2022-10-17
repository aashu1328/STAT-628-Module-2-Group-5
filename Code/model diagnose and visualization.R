library(tidyverse)
library(ggplot2)
fat=read.csv('BodyFat_Clean.csv')
model<-lm(BODYFAT~ABDOMEN+HEIGHT,data=fat) # 1st
summary(model)
plot(model)

## Independence
### There is no significant pattern for standardized residuals(r_i/sd), like autocorrelation or clustering.
plot(rstandard(model), type = "l",ylab='Standardized Residual')

## Homoscedasticity and Linearity
### No pattern for standardized residual(r_i/sd), no para-curve, high order of predictors does not matter a lot.
plot(rstandard(model),ylab='Standardized Residual') + abline(h = 0)

## Normality
qqnorm(rstandard(model))


## plots for shiny
fat%>%ggplot(aes(x=ABDOMEN,y=BODYFAT))+
  geom_smooth(se=0)+
  scale_x_continuous(name="Abdomen(cm)") +
  scale_y_continuous(name="Bodyfat(%)")
fat%>%ggplot(aes(x=BODYFAT))+
  geom_histogram(aes(y=..density..),color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name="Bodyfat(%)") +
  scale_y_continuous(name="Density")
