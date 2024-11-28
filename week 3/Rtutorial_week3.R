#R-Hour Week 3####
#Author: Ryan Fisher 
#Edits by Jordan Rustad
#Date: November 28, 2024

library(dplyr)

#Load data set####

# Ryan's file path
#lifedata <- read.csv("C:/Users/rfisher3/OneDrive - Government of Saskatchewan/Rtutorial_files/Week3/Life Expectancy Data.csv", stringsAsFactors=TRUE)

#relative path in project
lifedata <- read.csv("./week 3/Life Expectancy Data.csv")

#Analyses####
#Amazing resource with code on a multitude of 
#different stats tests: https://stats.oarc.ucla.edu/other/dae/

##T-TEST####
#Use a built in R dataset called sleep
#useful test if there are only 2 groups 

sleep 
#we're going to use a dataset that's "built into" R, so we don't have to
#import anything
#data set is in long format
#all t test needs is an x column and a y column

t.test(x=sleep$extra[sleep$group==1],y=sleep$extra[sleep$group==2])
#test shows that groups are not significantly different 

##CORRELATION####
#using lifedata - information on life expectancy related to different 
#variables 

#use this to find out if variables are correlated
#is life expectancy correlated to alcoholism? 
cor(lifedata$Adult.Mortality,lifedata$Alcohol)

#first run gives a result of NA; because some observations are missing!

cor.test(x=lifedata$Adult.Mortality,y=lifedata$Alcohol)

#There must be NAs in the dataset somewhere and so we need to tell the cor procedure to
#ignore them

cor(lifedata$Adult.Mortality,lifedata$Alcohol,use="complete.obs")

#Do correlation for more than one variable
#use cor and list the columns first column:last column
#follows order of what is in data frame 
cor(select(lifedata,Life.expectancy:Measles),use="complete.obs")

#easy way to run correlations 
#a package "corr" lets you create correlations between variables and plot it 


##REGRESSION####
#every package with a type of regression, mostly uses the same syntax
#order might vary a little

reg_example<-lm(Life.expectancy~Hepatitis.B+Measles+BMI, data=lifedata) 
#almost all models follow the same "setup" 
summary(reg_example)
options(scipen=999) #Turn off scientific notation, the e
summary(reg_example)
confint(reg_example) #To get confidence intervals of parameter estimates
#defaults to 95% CI 

#Regression model diagnostics
par(mfrow=c(2,2))
plot(reg_example)
#mine did not work?; when the window is too small the function won't work

#Visualize quickly what the model is predicting (not great for publication graphs, 
#but good for quick "looks" at the result)

install.packages("visreg")
library(visreg)

BMI_plot<-visreg(reg_example,"BMI") 
#holds every other variables at average/mean values but changes BMI
plot(BMI_plot)
HepB_plot<-visreg(reg_example,"Hepatitis.B") 
#holds every other variable at average values, but changes Hepatitis.B

#Can use information from visreg to make better plots in something like ggplot
#look at BMI_plot and extract the information
BMI_plot_data<-BMI_plot$fit

#linear model with an interaction
lifedata$Year<-as.factor(lifedata$Year)
reg_example_int<-lm(Life.expectancy~Hepatitis.B+Measles+BMI+Year+Year*BMI, data=lifedata)
summary(reg_example_int)
BMIint_plot<-visreg(reg_example_int,"BMI",by="Year")

#linear models with quadratics 
q_example<-lm(Life.expectancy~Hepatitis.B+Measles+BMI+I(Hepatitis.B^2), data=lifedata)
summary(q_example)
visreg(q_example)

#Mixed effects regression
install.packages("lme4")
library(lme4)

regmixed_example<-lmer(Life.expectancy~Hepatitis.B+Measles+BMI +(1|Country), data=lifedata) 
#where country is a random effect
#almost all models follow the same "setup", so knowing how to set up the basic regression 
#will help with everything else
summary(regmixed_example)

#Other resources#### 

#Correlation tests: https://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r 
#this checks if the two x variables in your regression are related; if they are too 
#closely related you should only keep one