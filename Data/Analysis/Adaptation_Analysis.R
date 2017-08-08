#Adaptation - previous concentration of TMA is affecting the current rating. 
#testing for this
#Import Libraries#####
library(reshape2)
library(ggplot2)
library(plyr)
library(plotrix)
library(ggplot2)
library(reshape2)
library(plyr)
library(drc)#this one is for the dose response curve
library(tidyverse)
library(kimisc)
library(data.table)
library(combinat)
library(gtools)
library(wesanderson)
library(platetools)
library(ggplot2)
library(viridis)

#Import Data####
subj <- dir(path = "Raw Data/TMA_DR_jars", pattern="\\.txt$", full.names=TRUE) # creating a list of all file names
names(subj)  <- basename(subj)
df <- ldply(subj, read.delim, stringsAsFactors=FALSE) 

#subset the data
df.jars <- subset(df, select = c("Subject", "Trial", "TrainList", "Running.Block.", "IntensityRating","Jar","Concentration")) 
df.jars$IntensityRating.inv <- 580-as.integer(df.jars$IntensityRating)

#Trial Data
df.trialJars <- subset(df.jars, Running.Block. == "Test", select = c("Subject", "Trial",  "Running.Block.", "IntensityRating.inv","Jar","Concentration"))

####Add column for previous concentration #####
df.trialJars$prevTrial <- df.trialJars$Trial - 1

getConcentrationPrevTrial <- function(subTrials){
  for(i in 1:length(subTrials$Trial)){
    if (subTrials$prevTrial[i] > 1){
      subTrials$prevConc[i] <- subTrials$Concentration[which(subTrials$Trial == subTrials$prevTrial[i])]
      
    } else{
      subTrials$prevConc[i] <- 0
    }
  }
  return(subTrials)
}

#loop through subjects to get concentration and then combine the dataframes
getConcentrationFinal <- function(df.trialJars){
  rm(df.prevConc)
  for (i in unique(df.trialJars$Subject)){
    subTrials <- subset(df.trialJars, Subject == i)
    subTrials.Concentration <- getConcentrationPrevTrial(subTrials)
    if(exists("df.prevConc")){
      df.prevConc <- rbind(df.prevConc, subTrials.Concentration)
    } else{
      df.prevConc <- subTrials.Concentration
    }
  }
  return(df.prevConc)
}
rm(df.prevConcFinal)
df.prevConcFinal <- getConcentrationFinal(df.trialJars)

#####Analyze the data by previous concentration#####
ggplot(df.prevConcFinal, aes(x = prevConc, y = IntensityRating.inv)) +
  geom_point()+
  facet_wrap(~Subject)

#this doesn't show anything alone. 


#####Normalize by concentration and graph against prev concentration#####
#Scale by concentration and subject
df.scaleByConcentration <- ddply(df.trialJars, .variables = c("Subject", "Concentration"), .fun = mutate, scale(IntensityRating.inv))
names(df.scaleByConcentration) = c("Subject", "Trial","Running.Block.","IntensityRating.inv","Jar","Concentration","prevTrial","scaleRating")
df.scaleByConcentration$prevTrial <- df.scaleByConcentration$Trial - 1
#rm(df.prevConc)
#rm(df.scale.prevConc)
df.scale.prevConc <- getConcentrationFinal(df.scaleByConcentration)
#graph data
ggplot(df.scale.prevConc, aes(x = prevConc, y = scale(IntensityRating.inv))) +
  geom_point()+
  facet_wrap(~Subject) +
  scale_x_log10()

#This doesn't look like anything either. This is somewhat promising, but it still doesn't explain all the weird stuff we are seeing. 


