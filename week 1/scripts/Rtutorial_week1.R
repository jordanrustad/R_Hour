#R Hour 
#Author: Ryan Fisher
#Notes and edits made by Jordan Rustad
#October 24, 2024

##'R Introduction - R is a open-source programming language, that for ecologists 
##'is primarily used for data analysis, visualizing results, mapping, and sometimes 
##'even creating reports/theses

##R studio Introduction - R Studio is a "point and click" (GUI) for R

##Quick tour of R studio
# Four Main Windows
  #1 Top left - Script editor (create, save, edit scripts/code)
  #2 Top Right - Workspace environment and history 
  #3 Bottom Left - R Console
  #4 Files, plots - packages, help and viewer pane (bottom right hand corner)

## What are R packages?
#' Base R (what you've installed already) has some"built-in" functionality. 
#' Packages are collections of R functions, data, and code to add specific 
#' functionality. 

# Some examples:
  # ggplot2 - make publication-ready graphs, complete control of graphing process
  # weathercan - access Government of Canada data from weather stations
  # raster - use spatial data in raster format
  # landscapemetrics - quantify landscapes from spatial data
  # lme4 - conduct linear-mixed effects modelling
  # rinat - get data from iNaturalist
  # baRcodeR - generate barcodes, QRcodes, etc
  # etc etc etc 

#'It can be hard to find R packages, but with some strategic googling, 
#'you can find almost anything
#'be specific about what you want in a package

  # Other Resources: 
  #            https://www.uvm.edu/~tdonovan/RforFledglings/
  #            https://stats.oarc.ucla.edu/r/
  #            https://drmattg.github.io/REcol-verse/
  #            https://www.davidzeleny.net/anadat-r/doku.php/en:r

#'For asking questions:
  #            Ecology in R Facebook group            
  #            https://stackoverflow.com/ (more for data manipulation)

##Few other quick things, then it's probably easier doing rather than listening!
  # Global Options
  # R Studio cheatsheet ("R Studio the company" is called Posit so you might see some things like "Posit Support", "Posit cheatsheets" etc)

##IMPORTING DATA
  #' There are a few ways to import data and typically depends on what format of data you have. 
  #' Note, you are essentially bringing in a copy of data into R Studio, so there is generally 
  #' no possibility of affecting/changing the original dataaset
  
  #' 1. for .csv or Excel files, point and click using the "Import Dataset" option in the 
  #' Workspace Environment window (but remember to copy code!! And note weird formatting of 
  #' filepath!)). Also not 'format" of a typical R command
  # Import Fulldataset.xlsx

#Can use the code generated from Import Dataset for future runs as well 

library(readxl) #tells R what packages you need for the code; loads package
nestdata <- read_excel("week 1/data/Fulldataset.xlsx") #saves data set to Global environment as 'nestdata'
View(nestdata) #view data frame

#A note about file paths: I am working in an R project with a set working directory
#Ryan is not using a project so the full path from the C drive is shown

#To install package, go to bottom right hand pane, click Packages, click Install Packages
#search for the package you need 
#alternatively, you can use install.packages() 
#example: install.packages("readxl")

# 2. What if your data is in google sheets? NOTE THAT R IS CASE SENSITIVE
#First download googlesheets4 package

install.packages("googlesheets4")
#a messge in the console pops up; Type Y to confirm download

#Enable/Turn on googlesheets4 package

library(googlesheets4)

#only Ryan has access to this particular sheet
newdata<-read_sheet("https://docs.google.com/spreadsheets/d/147O8BFbwMSGz7l8gi0m_ju500ZUkj7kapYy7IIp4J0o/edit?usp=sharing")
#an error occurs due to lack of permission 

#if I try with a sheet that I own:
newdata <- read_sheet("https://docs.google.com/spreadsheets/d/1qYyCyxpBTqzzFsJg_Xl3z_EJP5kW-Ls5m3wZzBNgB0I/edit?gid=0#gid=0")

rm(newdata) #removes data from Global Environment

## VIEW DATA
#1. Sort, filter in window - but can only view, cannot make changes to 
#data like in Excel

#Other commands

str(nestdata) #shows column including data type, and a couple values
nrow(nestdata) #returns number of rows, should match Global Environment
ncol(nestdata) # returns number of columns, should match Global Environment
dim(nestdata) #returns dimension of data frame; number of rows and columns
names(nestdata) #returns column names


## EXAMINING, MANIPULATING AND MANAGING DATA

# 1. Add new variable
#Make a new variable where the value is 5 for all observations
# Use the $ sign to specify a specific column
nestdata$weight<-5 
#data frame of interest selecting a new column and assigning all a single value

#Make a new variable to convert my vegetation height measurement from 
#mm to cm
nestdata$VEGHTmm <- nestdata$VEGHT * 100

#Oops I can't count, I need to remake that variables because there are only 10mm in a cm
nestdata$VEGHTmm<-nestdata$VEGHT * 10

#Let's make another new variable based on a condition

nestdata$TrueFalse<-ifelse(nestdata$VEGHTmm>=200,"TRUE","FALSE")
#R accepts True and False as values
#can also use other logical arguments such as greater or less than
#See an R cheat sheet for full Boolean symbols and arguments 

#2. Change a variable name

names(nestdata) #Remind ourselves what our variable names actually are
names(nestdata)[names(nestdata)=="VEGHTmm"] <- "veghtmm" 
# Square brackets mark the edges of a cell, column or row
names(nestdata)

#3. Now I want to remove some observations based on different criteria

library(dplyr)
nestdata_filtered <- filter(nestdata,RANDOM==1) # Keep any data where RANDOM is equal to 1
rm(nestdata_filtered)

nestdata_noNA<-filter(nestdata,!is.na(UTMX)) #Remove data with NA (i.e., no data)
rm(nestdata_noNA)

nestdata_filtered_string1<-filter(nestdata,NEST=="N32ALK") # Only keeps records where NEST column is equal to N32ALK
rm(nestdata_filtered_string1)

library(stringr)
nestdata_ALK<-filter(nestdata,str_detect(NEST,"ALK")) 
# Only keep observations in the NEST column that contain the string ALK (can't do this with base R)
rm(nestdata_ALK)

--------------------------------------

#'4. Let's say I have a variable that is coded as 0 or 1, but it's not actually representing a number/count
# but rather two groups of data. We need to "tell" R that this is a categorical variable and not a numeric

str(nestdata) #Note that NestCode is only 0s or 1s (in actuality this represents whether a site is a nest or a random site)
nestdata$NestCode<-as.factor(nestdata$NestCode)
str(nestdata)

#5 What if I had information in another dataset that I want to join to this one
#Import mergdataset.xlsx
mergedataset <- read_excel('week 1/data/mergedataset.xlsx')

str(mergedataset) #Note that the column FIELD is common between the new dataset and the one we've been working with
mergedataset_new<-merge(nestdata,mergedataset,all.x=TRUE)

#6 What if I had another dataset that I just wanted to append/stack on the current one
#Import appendataset_misscols
str(appenddataset_misscols)
complete_dataset<-rbind(nestdata,appenddataset_misscols)

#Import appenddataset
appenddataset <- read_excel('week 1/data/appenddataset.xlsx')
str(appenddataset)
complete_dataset<-rbind(nestdata,appenddataset)

#Now let's save that new dataset after combining
write.csv(new_appenddataset2,"XXXXX")


#TRANSPOSING,MOVING DATA AROUND, SUMMARIZING
#Import Fulldataset_wide.xlsx

#'1 We first want to make the columns CNORT, CSOUT, CWEST, CEAST into two columns. One columns indicating 
#'what measurement it is and then a column with the measurement itself (going from "wide" data to "long")
#Great resource - https://seananderson.ca/2013/10/19/reshape/

#Download package reshape2

install.packages("reshape2")
library(reshape2)

#Load fulldataset_wide.xlsx

fulldataset_wide <- read_excel('week 1/data/Fulldataset_wide.xlsx')

str(fulldataset_wide)
longdata<-melt(fulldataset_wide,id=c("YEAR","JULIAN","FIELD","HABITAT","NEST")) 
#the rest of the columns are what get flipped; goes from 5 columns to 2 

#rename the variable and value fields
names(longdata)[names(longdata)=="variable"]<-"Direction"
names(longdata)[names(longdata)=="value"]<-"cover"

#And then let's go backward
widedata<-dcast(longdata, YEAR+JULIAN+FIELD+HABITAT+NEST~Direction,value.var="cover")

#list variables that remain unchanged and then the ~ for the variable we want wide and which value

#* Be careful with some of these functions and always verify they are doing what you want them to do. 
#* Null values, NAs can sometimes royally screw these functions up
#* especially when flipping from long to wide

#2. Lastly let's do some summarizing of data
#I want to know the average of CNORT, CSOUT, etc by YEAR and FIELD combination

aggregate_YEARFIELD<-aggregate(x=fulldataset_wide, by=list(fulldataset_wide$YEAR,fulldataset_wide$FIELD), FUN="mean")

aggregate_HABITAT<-aggregate(x=fulldataset_wide,by=list(fulldataset_wide$HABITAT),FUN="mean")
#big difference between errors and warnings; warnings usually still run function and errors are a critical failure

#Take home message; use the resources; it has all the ways that you can use reshape2 

