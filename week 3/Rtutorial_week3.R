#R-Hour Week 3####
#Author: Ryan Fisher 
#Edits by Jordan Rustad
#Date: November 28, 2024

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

sleep #we're going to use a dataset that's "built into" R, so we don't have to import anything
t.test(x=sleep$extra[sleep$group==1],y=sleep$extra[sleep$group==2])

##CORRELATION####

cor(lifedata$Adult.Mortality,lifedata$Alcohol)
cor.test(x=lifedata$Adult.Mortality,y=lifedata$Alcohol)

#There must be NAs in the dataset somewhere and so we need to tell the cor procedure to
#ignore them

cor(lifedata$Adult.Mortality,lifedata$Alcohol,use="complete.obs")

#Do correlation for more than one variable
cor(select(lifedata,Life.expectancy:Measles),use="complete.obs")

##REGRESSION####

reg_example<-lm(Life.expectancy~Hepatitis.B+Measles+BMI, data=lifedata) 
#almost all models follow the same "setup" 
summary(reg_example)
options(scipen=999) #Turn off scientific notation
summary(reg_example)
confint(reg_example) #To get confidence intervals of parameter estimates

#Regression model diagnostics
par(mfrow=c(2,2))
plot(reg_example)

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
BMI_plot_data<-BMI_plot$fit

#linear model with an interaction
lifedata$Year<-as.factor(lifedata$Year)
reg_example_int<-lm(Life.expectancy~Hepatitis.B+Measles+BMI+Year+Year*BMI, data=lifedata)
summary(reg_example_int)
BMIint_plot<-visreg(reg_example_int,"BMI",by="Year")


#Mixed effects regression
library(lme4)
regmixed_example<-lmer(Life.expectancy~Hepatitis.B+Measles+BMI +(1|Country), data=lifedata) 
#almost all models follow the same "setup", so knowing how to set up the basic regression 
#will help with everything else
summary(regmixed_example)



