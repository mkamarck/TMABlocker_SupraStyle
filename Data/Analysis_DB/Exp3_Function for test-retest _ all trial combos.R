library(reshape2)
library(ggplot2)
library(plyr)
library(drc)

#Import Data_ Exp3####
subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/DR_Agitated", pattern="\\.txt$", full.names=TRUE) # creating a list of all file names
names(subj)  <- basename(subj)
df.jars.agit <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
df.jars.agit <- subset(df.jars.agit, select = c("Subject","Session", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating")) 
df.jars.agit$IntensityRating.inv <- 580-as.integer(df.jars.agit$IntensityRating)


#Trial Data
df.trialJars <- subset(df.jars.agit, Running.Block. == "Test", select = c("Subject", "Session", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))
#write.csv(df.trial, "TMA_supra_9.csv") #give this to Dardalie to import


#import the file that has the trial types
valve.list <- read.csv("//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_DR_merge.csv")

#merge with trial list
df.trial_merge <- merge(df.trialJars, valve.list)
#Remove sub.1000
Exp3retest.df <- subset(df.trial_merge, Subject!=1000)


correl.coeff <- function(Exp3retestT1, Exp3retestT2){
  Exp3retestT1 <- Exp3retestT1[,-3] #get rid of column w/ trial and Session numbers
  Exp3retestT2 <- Exp3retestT2[,-3]
  names(Exp3retestT1) = c("valve", "Subject", "valve2", "Running.Block.", "trial1", "TMA.Concentration")
  names(Exp3retestT2) = c("valve", "Subject", "valve2", "Running.Block.", "trial2", "TMA.Concentration")
  Exp3Trials.comb <- merge(Exp3retestT1, Exp3retestT2, by.x = c("valve", "Subject", "valve2", "Running.Block.", "TMA.Concentration"), by.y = c("valve", "Subject", "valve2", "Running.Block.", "TMA.Concentration"))#merge the dataframes
  testRetest.exp3 <- ddply(Exp3Trials.comb, .variables = c("Subject"), function(x) trial.cor = cor(x$trial1, x$trial2))
  testRetest.exp3$trial.corr <- abs(testRetest.exp3$V1)#takes abs. value of correlation values
  return(testRetest.exp3)
}

#Exp3retest.df <- subset(Exp3retest.df, select = c("Subject", "Trial", "IntensityRating.inv", "TMA.Concentration"))
Exp3retestT1 <- subset(Exp3retest.df, Trial <=26)
Exp3retestT2 <- subset(Exp3retest.df, Trial > 26)

#run the test-retest function
testRetest.exp3 <- correl.coeff(Exp3retestT1, Exp3retestT2)

#put subjects into order
testRetest.exp3$SubjectOrder <- factor(testRetest.exp3$Subject, levels = c(11, 50, 29, 33, 56, 49, 59, 58, 62, 54, 53), labels = c(1,2,3,4,5,6,7,8,9,10,11))

ggplot(testRetest.exp3, aes(x = SubjectOrder, y = trial.corr)) +
  geom_point()  +
  ylim(0,1)

#for comparison with experiment 1
testRetest.Exp3 <- testRetest.exp3
testRetest.Exp3$Experiment <- 3


