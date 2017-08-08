#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
#library(pipeR)
library(drc)

#Import Data
subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
  #subj <- dir(path = "/Volumes/mainland/Projects/TMA\ blocker/SupraStyle/Data/Raw\ Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 

#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#import the file that has the trial types
valve.list <- read.csv("//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a pc
  #valve.list <- read.csv("/Volumes/mainland/Projects/TMA\ blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a mac

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)

#10 are dardalie;  7 is ALice; and 4 is the first participant we ran today.
# test <- subset(df.trial, Subject == 10)
# test[which(test$valve == 32),]
ggplot(data = subset(df.trial_merge, Subject == 24), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Type)
#everyone (not normalized)
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

#graphing TMA alone
ggplot(data = subset(df.trial_merge, Type == "TMA"), aes(x = Concentration, y = IntensityRating.inv)) +
  geom_point() +
  facet_wrap(~Subject)

ggplot(data = subset(df.trial_merge, Type == "Linalool"), aes(x = Concentration, y = IntensityRating.inv)) +
  geom_point() +
  facet_wrap(~Subject)

#normalize data
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}
#returns dataframe with the minimum and maximum values for each subject
subjMinMax <- ddply(df.trial_merge, .variables = c("Subject"), .fun = summarize, minValue = min(IntensityRating.inv), maxValue = max(IntensityRating.inv))
#merges the max and min values with the full dataframe
df.scale <- merge(df.trial_merge, subjMinMax)
#calculates the normalized values using the scale function as written
df.scale$normValue <- scale01(df.scale$IntensityRating.inv, df.scale$minValue, df.scale$maxValue)

#graph normalized data
ggplot(data = subset(df.scale, Type == "TMA"), aes(x = Concentration, y = normValue)) +
  geom_point() +
  ggtitle(paste("TMA"))#+
  #facet_wrap(~Subject)

ggplot(data = subset(df.scale, Type == "Linalool"), aes(x = Concentration, y = normValue)) +
  geom_point()+
  ggtitle(paste("Linalool"))#+
  #facet_wrap(~Subject)
  

ggplot(data = subset(df.scale, Type == "Nonenol"), aes(x = Concentration, y = normValue)) +
  geom_point() +
  ggtitle(paste("Nonenol"))
  #facet_wrap(~Subject)

png("figures/allSubjectsNormalizedTMAvLinalool.png", width = 250, height = 250)
ggplot(data = subset(df.scale, Type %in% c("TMA", "Linalool")& !Subject%in%c("14","16","18","36")), aes(x = Concentration, y = normValue)) +
  geom_point() +
  ggtitle("Linalool and TMA Ratings") +
  facet_wrap(~Type)+
  xlab("Concentration (Arbitrary Units)") + 
  ylab("Normalized Intensity Rating")+
theme_bw()
dev.off()

#& Subject%notin% c("14","16","18","36")