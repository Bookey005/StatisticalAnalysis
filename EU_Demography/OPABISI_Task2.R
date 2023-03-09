# import required libraries
library(tidyverse)
library(corrplot)
library(psych)
library(outliers)
library(dplyr)
library(correlation)
library(RVAideMemoire)
library(rstatix)

install.packages("TTR")
install.packages("forecast")
install.packages("multcomp")
install.packages("ggpubr")
install.packages("stats")
install.packages("RVAideMemoire")
install.packages("correlation")
install.packages("tidyverse")
install.packages("psych")
install.packages("corrplot")
install.packages("leaps")
install.packages("caTools")    # For Linear regression 
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
install.packages("outliers")
install.packages("datarium")
install.packages("qqplotr")
install.packages("dplyr")
install.packages("Shapiro-wilk")
install.packages("ggplot2")


# set working directory and import data
Demodata <- read.csv("Demo_R.csv", header= TRUE)

#Data inspection
head(Demodata)# check the imported data
names(Demodata)
tail(Demodata)
str(Demodata)

#check for missing values
sum(is.na(Demodata)) #dataset contains NO Missing Values.
summary(is.na(Demodata))

# outlier detection Using BoxPlot in the numeric/continuous data columns
set_plot_dimensions <- function(width_choice , height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}

set_plot_dimensions(20,10)
par(mfrow=c(2,3))
boxplot(Demodata$Lifeexpectancy,
        ylab = "Life Expectancy",
        main = "Life Expectancy boxplt",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Demodata$Population_total,
        ylab = "Population_total",
        main = "Population_total boxplt",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Demodata$Netmigrationrate,
        ylab = "Netmigrationrate",
        main = "Netmigrationrate boxplt",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Demodata$Fertilityrate,
        ylab = "Fertilityrate",
        main = "Fertilityrate boxplt",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(Demodata$naturalincrease,
        ylab = "naturalincrease",
        main = "naturalincrease boxplt",
        col= "#FF6666",
        outcol="#FF6666")

#checking outliers values on variables
boxplot.stats(Demodata$naturalincrease)$out
boxplot.stats(Demodata$Netmigrationrate)$out
boxplot.stats(Demodata$Lifeexpectancy)$out

#checking the row with the outlier
out = boxplot.stats(Demodata$Lifeexpectancy)$out
outlier = which(Demodata$Lifeexpectancy %in% c(out))
outlier

out = boxplot.stats(Demodata$Netmigrationrate)$out
outlier = which(Demodata$Netmigrationrate %in% c(out))
outlier

out = boxplot.stats(Demodata$naturalincrease)$out
outlier = which(Demodata$naturalincrease %in% c(out))
outlier



#create a new dataframe and filter "Demodata" to contain each countries and their respective data
Demo_Austria<-Demodata[Demodata$Country=="Austria",]
Demo_Belgium<-Demodata[Demodata$Country=="Belgium",]
Demo_Estonia<-Demodata[Demodata$Country=="Estonia",]
Demo_Finland<-Demodata[Demodata$Country=="Finland",]
Demo_France<-Demodata[Demodata$Country=="France",]
Demo_Germany<-Demodata[Demodata$Country=="Germany",]
Demo_Greece<-Demodata[Demodata$Country=="Greece",]
Demo_Ireland<-Demodata[Demodata$Country=="Ireland",]
Demo_Italy<-Demodata[Demodata$Country=="Italy",]
Demo_Spain<-Demodata[Demodata$Country=="Spain",]




# Year and Country are removed as they are not relevant for the main question of the analysis.
Demodata4 <- subset(Demodata, select = -c(Year))
Demo_Austria2 <- subset(Demo_Austria, select = -c(Country, Year))
Demo_Belgium2 <- subset(Demo_Belgium, select = -c(Country, Year))
Demo_Estonia2 <- subset(Demo_Estonia, select = -c(Country, Year))
Demo_Finland2 <- subset(Demo_Finland, select = -c(Country, Year))
Demo_France2 <- subset(Demo_France, select = -c(Country, Year))
Demo_Germany2 <- subset(Demo_Germany, select = -c(Country, Year))
Demo_Greece2 <- subset(Demo_Greece, select = -c(Country, Year))
Demo_Ireland2 <- subset(Demo_Ireland, select = -c(Country, Year))
Demo_Italy2 <- subset(Demo_Italy, select = -c(Country, Year))
Demo_Spain2 <- subset(Demo_Spain, select = -c(Country, Year))





# descriptive statistcal analysis
#computing statistical analysis for the main dataset
desc_Demodata= psych::describe(Demodata)
desc_Demodata.t=t(desc_Demodata)#transpose the output
(desc_Demodata.t)

#computing statistical analysis for all the countries individually
desc_Demo_Austria= psych::describe(Demo_Austria2)
desc_Demo_Austria.t=t(desc_Demo_Austria)#transpose the output
(desc_Demo_Austria.t)

desc_Demo_Belgium= psych::describe(Demo_Belgium2)
desc_Demo_Belgium.t=t(desc_Demo_Belgium)#transpose the output
(desc_Demo_Belgium.t)

desc_Demo_Estonia= psych::describe(Demo_Estonia2)
desc_Demo_Estonia.t=t(desc_Demo_Estonia)#transpose the output
(desc_Demo_Estonia.t)

desc_Demo_Finland= psych::describe(Demo_Finland2)
desc_Demo_Finland.t=t(desc_Demo_Finland)#transpose the output
(desc_Demo_Finland.t)

desc_Demo_France= psych::describe(Demo_France2)
desc_Demo_France.t=t(desc_Demo_France)#transpose the output
(desc_Demo_France.t)

desc_Demo_Germany= psych::describe(Demo_Germany2)
desc_Demo_Germany.t=t(desc_Demo_Germany)#transpose the output
(desc_Demo_Germany.t)

desc_Demo_Greece= psych::describe(Demo_Greece2)
desc_Demo_Greece.t=t(desc_Demo_Greece)#transpose the output
(desc_Demo_Greece.t)

desc_Demo_Ireland= psych::describe(Demo_Ireland2)
desc_Demo_Ireland.t=t(desc_Demo_Ireland)#transpose the output
(desc_Demo_Ireland.t)

desc_Demo_Italy= psych::describe(Demo_Italy2)
desc_Demo_Italy.t=t(desc_Demo_Italy)#transpose the output
(desc_Demo_Italy.t)

desc_Demo_Spain= psych::describe(Demo_Spain2)
desc_Demo_Spain.t=t(desc_Demo_Spain)#transpose the output
(desc_Demo_Spain.t)

# comparing the indicator average across per countries
Demodata_classify <- Demodata4 %>% group_by(Country)
Demodata_classy <- dplyr::summarise(Demodata_classify, AveragePopulation_total = mean(Population_total), 
                                    AverageFertilityrate = mean(Fertilityrate),
                                    AverageLifeexpectancy = mean(Lifeexpectancy),
                                    AverageNetmigrationrate = mean(Netmigrationrate),
                                    Averagenaturalincrease = mean(naturalincrease))
(Demodata_classy)

# comparing the indicator standard deviation across per countries
Demodata_classify <- Demodata4 %>% group_by(Country)
Demodata_classy_sd <- dplyr::summarise(Demodata_classify, sdPopulation_total = sd(Population_total), 
                                       sdFertilityrate = sd(Fertilityrate),
                                       sdLifeexpectancy = sd(Lifeexpectancy),
                                       sdNetmigrationrate = sd(Netmigrationrate),
                                       sdnaturalincrease = sd(naturalincrease))
(Demodata_classy_sd)

# normality check per country 

set.seed(0)

shapiro.test(Demo_Belgium$Population_total)#we can assume normality because p-value is greater than 0.05
shapiro.test(Demo_Estonia$Population_total)#we can assume normality because p-value is greater than 0.05
shapiro.test(Demo_Germany$Population_total)#we can assume normality because p-value is greater than 0.05
shapiro.test(Demo_Ireland$Population_total)#we can assume normality because p-value is greater than 0.05



#PLOTTING POPULATIONTOTAL ACROSS ALL COUNTRIES

hist(Demo_Belgium2$Population_total)
plot(density(Demo_Belgium2$Population_total,na.rm=TRUE))#Right-skew distributed
hist(Demo_Estonia2$Population_total)
plot(density(Demo_Estonia2$Population_total,na.rm=TRUE))#Right-skew distributed
hist(Demo_Germany2$Population_total)
plot(density(Demo_Germany2$Population_total,na.rm=TRUE))#Right-skew distributed
hist(Demo_Ireland2$Population_total)
plot(density(Demo_Ireland2$Population_total,na.rm=TRUE))#left-skew distributed



# correlation analysis
cor_Austria = round(cor(Demo_Austria2)) # rounded to 2 decimals
#view(cor_Austria)
cor_Belgium = round(cor(Demo_Belgium2),
                    digits = 2 # rounded to 2 decimals
)
#view(cor_Belgium)
cor_Estonia = round(cor(Demo_Estonia2),
                    digits = 2 # rounded to 2 decimals
)
#view(cor_Estonia)
cor_Finland = round(cor(Demo_Finland2),
                    digits = 2 # rounded to 2 decimals
)
#view(cor_Finland)
cor_France = round(cor(Demo_France2),
                   digits = 2 # rounded to 2 decimals
)
#view(cor_France)
cor_Germany = round(cor(Demo_Germany2),
                    digits = 2 # rounded to 2 decimals
)
#view(cor_Germany)
cor_Greece = round(cor(Demo_Greece2),
                   digits = 2 # rounded to 2 decimals
)
#view(cor_Greece)
cor_Ireland = round(cor(Demo_Ireland2),
                    digits = 2 # rounded to 2 decimals
)
#view(cor_Ireland)
cor_Italy = round(cor(Demo_Italy2),
                  digits = 2 # rounded to 2 decimals
)
#view(Demo_Italy)
cor_Spain = round(cor(Demo_Spain2),
                   digits = 3 # rounded to 2 decimals
)
#view(cor_Spain)

#Correlation matrix showing only upper side
#select variables highly correlated values to Total population and its growth factors

corrplot::corrplot(cor_Austria,
         method = "number",  
         type = "upper",
         #tl.srt = 33
)
corrplot::corrplot(cor_Belgium,
         method = "number",  
         type = "upper",
         #tl.srt = 33# show only upper side
)
corrplot::corrplot(cor_Estonia,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)
corrplot::corrplot(cor_Finland,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)

corrplot::corrplot(cor_France,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)
corrplot::corrplot(cor_Germany,
         method = "number",
         type = "upper",
         #tl.srt = 33# show only upper side
)

corrplot::corrplot(cor_Greece,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)
corrplot::corrplot(cor_Ireland,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)
corrplot::corrplot(cor_Italy,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)

corrplot::corrplot(cor_Spain,
         method = "number", 
         type = "upper",
         #tl.srt = 33# show only upper side
)



#correlation coeficient
coef_test_Demo_Austria = correlation::correlation(Demo_Austria2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Austria)

coef_test_Demo_Belgium = correlation::correlation(Demo_Belgium2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Belgium)
coef_test_Demo_Estonia = correlation::correlation(Demo_Estonia2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Estonia)
coef_test_Demo_Finland = correlation::correlation(Demo_Finland2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Finland)
coef_test_Demo_France = correlation::correlation(Demo_France2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_France)
coef_test_Demo_Germany = correlation::correlation(Demo_Germany2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Germany)
coef_test_Demo_Greece = correlation::correlation(Demo_Greece2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Greece)
coef_test_Demo_Ireland = correlation::correlation(Demo_Ireland2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Ireland)
coef_test_Demo_Italy = correlation::correlation(Demo_Italy2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Italy)
coef_test_Demo_Spain = correlation::correlation(Demo_Spain2,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Spain)





#Regression analysis
# MULTIPLE LINEAR REGRESSION
Demo_Belgium<-Demodata[Demodata$Country=="Belgium",]
Demo_Belgium2 <- subset(Demo_Belgium, select = -c(Country, Year))
Demo_Belgium2_reduced <- Demo_Belgium2[, c('Population_total','Lifeexpectancy',
                                           'Netmigrationrate','naturalincrease',
                                           'Fertilityrate')]

cor_Belgium = round(cor(Demo_Belgium2_reduced),
                    digits = 2 # rounded to 2 decimals
)

corrplot::corrplot(cor_Belgium,
                   method = "number", 
                   type = "upper",
                   #tl.srt = 33# show only upper side
)

coef_test_Demo_Belgium = correlation::correlation(Demo_Belgium2_reduced,
                                                  include_factors = TRUE, method = "auto"
)
(coef_test_Demo_Belgium)

reg_Belgium = lm(Population_total ~ 
                   Fertilityrate
                 + Netmigrationrate, data = Demo_Belgium2_reduced)
summary.lm(reg_Belgium) #Population_total = 1773320 × Fertilityrate + 196753 × Netmigrationrate



pairs(Demo_Belgium2_reduced[,c(1,3,5)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Belgium, 1)
plot(reg_Belgium, 2)
plot(reg_Belgium, 3)

reg_Finland = lm(Population_total ~ Lifeexpectancy + Netmigrationrate, data = Demo_Finland2)
summary.lm(reg_Finland)
pairs(Demo_Finland2[,c(4,6,8)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Finland, 1)
plot(reg_Finland, 2)
plot(reg_Finland, 3)


reg_France = lm(Population_total ~ naturalincrease + Lifeexpectancy, data = Demo_France2)
summary.lm(reg_France)
pairs(Demo_France2[,c(4,6,9)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_France, 1)
plot(reg_France, 2)
plot(reg_France, 3)


reg_Germany = lm(Population_total ~ naturalincrease + 
                   Lifeexpectancy + Netmigrationrate, data = Demo_Germany2)
summary.lm(reg_Germany)#Population_total = -607337 × naturalincrease - 1345310 × Lifeexpectancy – 631038 × Netmigrationrate
pairs(Demo_Germany2[,c(4,6,7,9)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Germany, 1)
plot(reg_Germany, 2)
plot(reg_Germany, 3)


reg_Greece = lm(Population_total ~ naturalincrease + 
                  Lifeexpectancy + Netmigrationrate, data = Demo_Greece2)
summary.lm(reg_Greece)
pairs(Demo_Greece2[,c(4,6,7,9)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Greece, 1)
plot(reg_Greece, 2)
plot(reg_Greece, 3)


reg_Italy = lm(Population_total ~ naturalincrease + Lifeexpectancy, data = Demo_Italy2) # model
summary.lm(reg_Italy)
pairs(Demo_Italy2[,c(4,6,7,9)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Italy, 1)
plot(reg_Italy, 2)
plot(reg_Italy, 3)


reg_Spain = lm(Population_total ~ Lifeexpectancy + Netmigrationrate + naturalincrease, data = Demo_Spain2)
summary.lm(reg_Spain)
pairs(Demo_Spain2[,c(4,6,7,8,9)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(reg_Spain, 1)
plot(reg_Spain, 2)
plot(reg_Spain, 3)



# MULTICOLINEARITY TEST FOR MULTIPLE REGRESSION
car::vif(reg_Belgium)
car::vif(reg_Finland)
car::vif(reg_France)
car::vif(reg_Germany)
car::vif(reg_Greece)
car::vif(reg_Italy)
car::vif(reg_Spain)


# HYPOTHESES AND ANOVA TEST
#Comparing the if natural increase has a significant impact on population growth

Demodata_classify <- Demodata4 %>% group_by(Country)
Demodata_classify2 <- dplyr::summarise(Demodata_classify, 
                                       AveragePopulation_growth = mean(Population_growth), 
                                       Averagenaturalincrease = mean(naturalincrease))
Demodata_classify2

#Return dataframe average POPULATIONGROWTH and naturalincrease (low or high) as columns and each row corresponding to each country in the dataset.
x <- Demodata_classify2 %>% dplyr::filter (Averagenaturalincrease <=1.02)

z <- Demodata_classify2 %>% dplyr::filter (Averagenaturalincrease >1.02)

yNEW1<-data.frame(AveragePopulation_growth = x$AveragePopulation_growth)
yNEW1$naturalincrease = 'Low'

yNEW3<-data.frame(AveragePopulation_growth = z$AveragePopulation_growth)
yNEW3$naturalincrease = 'High'

Finaldemoclassify <-data.frame(rbind(yNEW1,yNEW3))
Finaldemoclassify

boxplot(AveragePopulation_growth  ~ naturalincrease , 
        data=Finaldemoclassify, names=c("LOW","HIGH"),
        xlab="Classes of naturalincrease", 
        ylab="naturalincrease Level",
        main="naturalincrease LEVEL over ten years")

# Compute the analysis of variance
output.aov <- aov(AveragePopulation_growth  ~ naturalincrease, data = Finaldemoclassify)

summary(output.aov) #As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the country highlighted with “*" in the model summary.

#multiple pairwise-comparison 'Turkey' takes the fitted ANOVA as an argument. check value less than 0.05
TukeyHSD(output.aov)

#pairwise-t-test used to calculate pairwise comparisons between category levels with corrections for multiple testing.
pairwise.t.test(Finaldemoclassify$AveragePopulation_growth, Finaldemoclassify$naturalincrease,
                p.adjust.method = "BH")

#  Homogeneity of variances
stats::bartlett.test(AveragePopulation_growth ~ naturalincrease, data = Finaldemoclassify)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different populationgrowth per country.


# normality assumptions
plot(output.aov, 2)  #all the points fall approximately along this reference line, we can assume normality.

# Extract the residuals
aov_residuals <- residuals(object = output.aov )

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

#The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals (W = 0.99306, p-value = 0.9992) which finds no indication that normality is violated.

# ANOVA test with no assumption of equal variances
oneway.test(AveragePopulation_growth ~ naturalincrease, 
            data = Finaldemoclassify, var.equal = TRUE)

#Var.equal = TRUE is for the assumption checked that all the variances are equal


# TIME - SERIES
# Least ranked EU country based on mean avaerage from the selected data
Population_totaltimeseries <- ts(Demo_Estonia$Population_total, 
                                 frequency=1, start=c(2010,1))
Population_totaltimeseries
plot.ts(Population_totaltimeseries)

####### Decomposing Non-Seasonal Data ######
Population_totaltimeseriesSMA <- TTR::SMA(Population_totaltimeseries,n=2)
plot.ts(Population_totaltimeseriesSMA)


#use Holt’s exponential smoothing
Population_totaltimeseriesforecasts <- HoltWinters(Population_totaltimeseries, 
                                                   gamma=FALSE)
Population_totaltimeseriesforecasts

Population_totaltimeseriesforecasts$fitted
Population_totaltimeseriesforecasts$SSE

plot(Population_totaltimeseriesforecasts)

#forecasting prediction model for 10 years period
Populationforecasts2 <- forecast:::forecast.HoltWinters(Population_totaltimeseriesforecasts, 
                                                                        h=10)
plot(Populationforecasts2)

#improving forecasted prediction model

acf(Populationforecasts2$residuals, lag.max=8, na.action = na.pass)
Box.test(Populationforecasts2$residuals, lag=8, type="Ljung-Box")

plot.ts(Populationforecasts2$residuals) # make time series plot

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm=TRUE)/4
  mysd   <- sd(forecasterrors,na.rm=TRUE)
  mymin  <- min(forecasterrors,na.rm=TRUE) - mysd*5
  mymax  <- max(forecasterrors,na.rm=TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(Populationforecasts2$residuals) # make a histogram

mean(Populationforecasts2$residuals)


# Arima model

Population_totaltimeseriesdiff1 <- diff(Population_totaltimeseries, differences=1)
plot.ts(Population_totaltimeseriesdiff1) #does not appear to be stationary in mean

Population_totaltimeseriesdiff2 <- diff(Population_totaltimeseries, differences=2)
plot.ts(Population_totaltimeseriesdiff2) #does appear to be stationary in mean and variance


Population_totaltimeseriesarima <- forecast:::auto.arima(Demo_Estonia$Population_total)# fit an ARIMA(1,0,0) model

Population_totaltimeseriesarima

Population_totaltimeseriesarimaforecasts <- forecast:::forecast.Arima(Population_totaltimeseriesarima,
                                                                      h=10) # forecast with confidence level 99.5%
Population_totaltimeseriesarimaforecasts

plot(Population_totaltimeseriesarimaforecasts)

# investigate forecast error
acf(Population_totaltimeseriesarimaforecasts$residuals, lag.max=8)
Box.test(Population_totaltimeseriesarimaforecasts$residuals, lag=8, type="Ljung-Box")

plot.ts(Population_totaltimeseriesarimaforecasts$residuals)            # make time plot of forecast errors
plotForecastErrors(Population_totaltimeseriesarimaforecasts$residuals)

mean(Population_totaltimeseriesarimaforecasts$residuals)
