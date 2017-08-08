#agitating the TMA jars 7/25/17 by bubbling through the liquid
#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)

#Import Data
subj <- dir(path = "Raw Data/DR_Agitated", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#name the trials based on valves
valve.list <- read.csv("Analysis/TMA_DR_merge.csv")

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)

#graph
ggplot(df.trial_merge, aes(x = TMA.Concentration, y = IntensityRating.inv, colour = factor(valve2))) +
  geom_point() +
  facet_wrap(~Subject)

#Figure out the order of the subjects so I can see if TMA decreases over time. 
#11 was first on 7-26-17
#33 and 50 were next (not sure the order)
#29 was after that, but also on 7/27
#the water alone seems to have some odor - or at least more odor than the DEP; might need to replace the water everyday