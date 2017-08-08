#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)
library(drc)
library(tidyverse)
library(kimisc)
library(data.table)
library(combinat)
library(gtools)

#Import Data
subj <- dir(path = "Raw Data", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 

#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))
#write.csv(df.trial, "TMA_supra_9.csv") #give this to Dardalie to import

#import the file that has the trial types
#valve.list <- read.csv("Analysis/TMA_supra_Pilot_merge.csv")
valve.list <- read.csv("Analysis/TMA_supra_highTMA_merge.csv")

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)


#10 are dardalie;  7 is ALice; and 4 is the first participant we ran today.
# test <- subset(df.trial, Subject == 10)
# test[which(test$valve == 32),]
ggplot(data = subset(df.trial_merge), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Type)

#Plot each person individually
for (i in unique(df.trial_merge$Subject)) {
  plot = ggplot(data = subset(df.trial_merge, Subject == i), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
    geom_point() +
    facet_wrap(~Type) +
    ggtitle(paste("Subject Number ", i))
  print(plot)
}

#refreshed odors on 7/11 after participants 10, 7, 4, and 16
