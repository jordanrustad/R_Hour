#R Hour - Week 2 
#Descriptive stats and visualizing data

library(readxl)
nestdata <- read_excel("week 1/data/Fulldataset.xlsx")
View(nestdata)

#DESCRIPTIVE STATISTICS IN R####

#Minimum/Maximum, mean, sd, se, range for individual variables
min(nestdata$LITTER)
max(nestdata$LITTER)
mean(nestdata$LITTER)
sd(nestdata$LITTER)
se(nestdata$LITTER) #this does not work
sd(nestdata$LITTER)/sqrt(length(nestdata$LITTER)) #SE is SD/SQRT 
range(nestdata$LITTER)

#Summary stats for entire data frame
summary(nestdata)

install.packages("vtable")
library(vtable)
st(nestdata) #creates summary table 
st(nestdata,vars=c('BG','FORB','LIVE','DEAD','VEGHT','LITTER'))  
#..c().. indicates that you are passing a list of things
st(nestdata,group='RANDOM',vars=c('BG','FORB','LIVE','DEAD','VEGHT','LITTER'))

#Creating a dataframe if you need averages from certain variables for analyses
nestdata_avgbyfield<-aggregate(nestdata$FORB, list(nestdata$FIELD),FUN=mean)

#What if I want to do this with all my variables
library(dplyr)

#summarise data grouped by habitat and random
nestdata_avgbyhabrand_allvars<-nestdata %>% group_by(HABITAT,RANDOM) %>% summarize('avg_fb'=mean(FORB),
                                                                                'avg_bg'=mean(BG),
                                                                                'avg_dead'=mean(DEAD),
                                                                                'avg_veght'=mean(VEGHT),
                                                                                'avg_litter'=mean(LITTER))

nestdata_SDbyhabrand_allvars<-nestdata %>% group_by(HABITAT,RANDOM) %>% summarize('sd_fb'=sd(FORB),
                                                                                   'sd_bg'=sd(BG),
                                                                                   'sd_dead'=sd(DEAD),
                                                                                   'sd_veght'=sd(VEGHT),
                                                                                   'sd_litter'=sd(LITTER))

#GRAPHING####

#PERHAPS THE MOST ANNOYING FUNCTIONS IN R ARE FOR GRAPHING
#Although let's start simple - base plotting functions in R (no additional packages needed)

#1
#https://sites.harding.edu/fmccown/r/
##Simple plot, 1 variable vs another####
plot(nestdata$BG,nestdata$VEGHT)

#Change axis titles
plot(nestdata$BG,nestdata$VEGHT,main="vegetation height vs bare ground",
     xlab="% bare ground",ylab="Vegetation height (mm)")

#Change color of points
plot(nestdata$BG,nestdata$VEGHT,col="blue",main="vegetation height vs bare ground",
     xlab="% bare ground",ylab="Vegetation height (mm)")

#Change size of points
plot(nestdata$BG,nestdata$VEGHT,col="blue",cex=0.5,main="vegetation height vs bare ground",
     xlab="% bare ground",ylab="Vegetation height (mm)")
#cex = size of points 

#Change symbol of points
plot(nestdata$BG,nestdata$VEGHT,col="blue",cex=0.5,pch=8,main="vegetation height vs bare ground",
     xlab="% bare ground",ylab="Vegetation height (mm)")
#pch = point character 

#Change symbols of points by another grouping variable and add legend
plot(nestdata$BG,nestdata$VEGHT,col=factor(nestdata$HABITAT),cex=0.5,pch=19,main="vegetation height vs bare ground",
     xlab="% bare ground",ylab="Vegetation height (mm)")
#col = colour based on what column, factor to tell R it's categorical
legend("topright",
       legend = levels(factor(nestdata$HABITAT)),
       pch = 19,
       col = factor(levels(factor(nestdata$HABITAT))))
#Note that I had to tell R that my habitat variable is a grouping/factor variable

#Export final graph
#Use Export button in Plots tab, but beware size issues!!

#What about a simple histogram
hist(nestdata$LIVE)


#2
##ggplot - the most annoying (but flexible) package####
# What I recommend is learning ggplot with a different package called ggplotgui that will let you make graphs "point and click" style and then writes the R code for you
install.packages("ggplot2")
install.packages("ggplotgui")
library(ggplot2)
library(ggplotgui)

ggplot_shiny(data=nestdata) #opens gui

#Copied from ggplotgui
graph <- ggplot(nestdata, aes(x = HABITAT, y = VEGHT)) +
  geom_violin(adjust = 1) +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  labs(x = 'label x-axis', y = 'Vegetation height (mm)') +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  )
graph

#How to save this
ggsave("./week 2/graph_week2.tif", graph, width = 14, height = 14, units = 'cm',dpi=300)

#Can add regressions to a plot as well
graph_reg <- ggplot(nestdata, aes(x = BG, y = VEGHT)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = '% bare ground', y = 'Vegetation height (mm)') +
  theme_classic()
graph_reg

#let's put the graphs together

install.packages("ggpubr")
library(ggpubr)

panel_graph <- ggarrange(graph, graph_reg)
panel_graph

#Other resources for graphing
## https://rstudio.github.io/cheatsheets/data-visualization.pdf
#Trial and Error - it's the only way to learn unfortunately 
