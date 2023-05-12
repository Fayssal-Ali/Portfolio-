Life<- read.csv('C:/Users/Fays/Downloads/Project/Life Expectancy Data.csv')

newyear <- Life[(Life$Year==2014),]
newyear<-newyear[c(1,2,4,8,11,14,17,21,22)]

#update<- life1[(life1$Year==2014),]
#newyear$BMI <- update$BMI
newyear1<-na.omit(newyear)


library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
library(lme4)

#Univariate Analysis
life_expect <- newyear1$Life.expectancy
describe(newyear1$Life.expectancy)
boxplot(newyear1$Life.expectancy)
hist(life_expect)

#Bivariate Analysis with Life Expectancy 
life_gdp <- lm(Life.expectancy~log(GDP), data = newyear1)
summary(life_gdp) 
confint(life_gdp)
#Heatmap 
newyear1$GDP_log <- log(newyear1$GDP)
heatplot_GDP <- cor(data.frame(newyear1$Life.expectancy, newyear1$GDP_log),
                    use = "pairwise.complete.obs")
heatplot_GDP 
#Correlation Plot
corrplot(heatplot_GDP,method="square", diag = T)
pairs(data.frame(newyear1$Life.expectancy, newyear1$GDP_log))
pairs.panels(data.frame(newyear1$Life.expectancy, newyear1$GDP_log),
             method="pearson", pch=20)

#Bivariate Analysis of other variables
heatplot_final <- cor(data.frame(newyear1$Life.expectancy, newyear1$GDP_log, newyear1$Schooling, newyear1$BMI),
                      use = "pairwise.complete.obs")
heatplot_final
#Correlation Plot
corrplot(heatplot_finakl,method="square", diag = T)
pairs(data.frame(newyear1$Life.expectancy, newyear1$GDP_log, newyear1$Schooling, newyear1$BMI))

#Show correlation coefficient and scatterplots together
pairs.panels(data.frame(newyear1$Life.expectancy, newyear1$`Total expenditure`, newyear1$Schooling, newyear1$BMI),
             method="pearson", pch=20)

#GDP & Life Expectancy - do ln
plot(Life.expectancy ~ GDP, data = newyear1,
     ylab= "Life expentancy (years)",
     xlab= "GDP(dollars)",
     main= "Life expectancy vs GDP",
     xlim = c(0, 80000), pch = 20, col="blue")
abline(lm(Life.expectancy~ GDP, data = newyear1))
#log transformation

plot(Life.expectancy ~ log(GDP), data = newyear1,
     ylab= "Life expentancy (years)",
     xlab= "Log(GDP)",
     main= "Life expectancy vs log(GDP)",
     xlim = c(0, 20), pch = 20, col="blue", 
     ylim = c(0,100))
abline(lm(Life.expectancy~log(GDP), data = newyear))

par(mfrow=c(1,1))
#Life Expect and BMI - very linear
plot(Life.expectancy ~ BMI, data = newyear1,
     ylab= "Life expentancy (years)",
     xlab= "BMI",
     main= "Life expectancy vs BMI",
     ylim = c(40, 100), pch = 16, col="blue",
     xlim= c(0,80))
abline(lm(Life.expectancy~BMI, data = newyear1))

#Life expect and Schooling - very linear
plot(Life.expectancy ~ Schooling, data = newyear,
     ylab= "Life expentancy (years)",
     xlab= "Schooling (years)",
     main= "Life expectancy vs Years of Schooling",
     ylim = c(0, 100), pch = 16, col="blue",
     xlim= c(0,25))
abline(lm(Life.expectancy ~ Schooling, data = newyear))


# total expediture - foggy
plot(Life.expectancy ~ Total.expenditure, data = newyear,
     xlab= "% Expenditure",
     ylab= "Life expectancy (years)",
     main= "Life expectancy vs total Expenditure",
     ylim = c(40, 100), pch = 16, col="blue")
abline(lm(Life.expectancy ~  Total.expenditure, data = newyear))



# MODEL FIT

# Reduced Model
fitr <- lm(Life.expectancy~1,data = newyear1)

# Full Model
fitf <- lm(Life.expectancy~log(GDP)+ Schooling + BMI + Total.expenditure ,data = newyear1)
AIC(fitf) # 923


# check to see if H0: All B=0 true
anova(fitr, fitf, test="F") # p 2.2e-16, so reject null 

# Type III test to check variables
drop1(fitf,~.,test = "F")
summary(fitf)
confint(fitf)
# remove stat insig var = total expenditure
fit <- lm(Life.expectancy~log(GDP)+ Schooling + BMI,data = newyear1)
drop1(fit,~.,test = "F")
summary(fit)
anova(fit, fitf, test="F") # p 0.3108, so fail to reject null 

# graph to check plot
y=(1:100)
x=(1:100)
par(mfrow=c(1,1))
plot( fitted(fit) ~ newyear1$Life.expectancy, 
      ylab= "Fitted Life Expectancy",
      xlab= "Life expectancy (years)",
      main= "Actual Vs. Fitted Values")
text(fitted(fit)~newyear1$Life.expectancy,data=newyear1,label=Country)
abline(lm(y~x))

# weights - no difference
w<-1/lowess(I(resid(fit)^2)~fitted(fit))$y
fit.wls<-lm(Life.expectancy~log(GDP)+ Schooling + BMI, data=newyear1,weights=w)


# test model assumptions

par(mfrow=c(1,2))
## resid plot
plot(resid(fit)~fitted(fit))
lines(lowess(resid(fit)~fitted(fit)))
## resid^2 plot
plot(I(resid(fit)^2)~fitted(fit),cex=.25,pch=16,data=newyear1)
#text(I(resid(fit)^2)~fitted(fit),data=newyear1,label=Country)
lines(lowess(resid(fit)^2~fitted(fit)))
mean(resid(fit))

## trying weighting method
par(mfrow=c(1,2))
## resid plot
plot(resid(fit.wls)~fitted(fit.wls))
lines(lowess(resid(fit.wls)~fitted(fit.wls)))
## resid^2 plot
plot(I(resid(fit.wls)^2)~fitted(fit.wls),cex=.25,pch=16,data=newyear1)
#text(I(resid(fit)^2)~fitted(fit),data=newyear1,label=Country)
lines(lowess(resid(fit.wls)^2~fitted(fit.wls)))


par(mfrow=c(1,2)) # to get it back to 1 plot
## histogram
hist(resid(fit))
## qqplot
qqnorm(resid(fit)); qqline(resid(fit))


#leverage/outliers
plot(rstudent(fit)~fitted(fit),type="n", ylim = c(-4,4))
text(rstudent(fit)~fitted(fit),data=newyear1,label=Country)
abline(h=c(-3,3))
#sierra Leone and Lesotho might be outliers (extreme on the y-axis)
# no real leverage points, maybe Australia (extreme on the x-axis)


#influential points
plot(rstudent(fit)~influence(fit)$hat,type="n",ylim=c(-5,5))
text(rstudent(fit)~influence(fit)$hat,data=newyear1,label=Country)
abline(h=c(-3,3),v=c(2,3)*2/nrow(newyear1)) #no u=influential points

par(mfrow=c(1,3))
# plot cook's distance, dfbetas, dffits, and covratio
plot(cooks.distance(fit)~fitted(fit),data=newyear1 ,type="n")
text(cooks.distance(fit)~fitted(fit),data=newyear1,label=Country)
segments(x0=fitted(fit),y0=0,fitted(fit),y1=cooks.distance(fit))
#Swaziland

plot(abs(dfbetas(fit)[,2])~fitted(fit),data=newyear1,type="n")
text(abs(dfbetas(fit)[,2])~fitted(fit),data=newyear1,label=Country)
segments(x0=fitted(fit),y0=0,x1=fitted(fit),y1=abs(dfbetas(fit)[,2]))
# maybe morocco, elsalvador

plot(abs(dffits(fit))~fitted(fit),data=newyear1,type="n")
text(abs(dffits(fit))~fitted(fit),data=newyear1,label=Country)
segments(x0=fitted(fit),y0=0,x1=fitted(fit),y1=abs(dffits(fit)))
# swaziland

plot(covratio(fit)~fitted(fit),data=newyear1,type="n")
text(covratio(fit)~fitted(fit),data=newyear1,label=Country)
segments(x0=fitted(fit),y0=1,x1=fitted(fit),y1=covratio(fit))
abline(h=1)
#abline(h=1+c(-3,3)*2/nrow(newyear1),lty=2)
abline(h=1+c(-1,1)*3*length(fit$coefficients)/nrow(newyear1),lty=2)




# # plot regression with and without points of concern
# # see slide 50
# par(mfrow=c(2,2))
# v<-c(50,1476, 2299)
# for(i in 1:length(v)){
#   inf.pts<-v[i]
#   fit2<-lm(Life.expectancy~log(GDP)+ Schooling + BMI, data = newyear1[-inf.pts,])
#   summary(fit2)
#   high.inf<-newyear1[inf.pts,]
#   low.inf<-newyear1[-inf.pts,]
#   plot(Life.expectancy~log(GDP)+ Schooling + BMI, data=low.inf,ylim=range(newyear1$BMI),xlim=range(newyear1$Life.expectancy))
#   lines(Life.expectancy~log(GDP)+ Schooling + BMI,data=high.inf,pch=16,type="p",col=2,cex=2)
#   abline(fit1,lwd=2)
#   abline(fit2,col=2,lwd=3,lty=2)
#   title(paste(v[i]," Deleted"))
# }




