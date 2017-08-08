#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)

#Import Data#########
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
###########################


#Working on Experiment 3 test-retest more. 

####################################################################################
Exp3retest.df <- subset(df.trial_merge, Subject!=1000)

#original test-retest function
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
####################################################################################
#for comparison with experiment 1 - rest of this code is in TMA Ratings_Exp1 vs Exp3.R
testRetest.Exp3 <- testRetest.exp3
testRetest.Exp3$Experiment <- 3
####################################################################################

#Split up the test-retest by 4 without the different valves

#make a new function
testRetest <- function(Trial1, Trial2){
  Trial1 <- Trial1[,c(-3,-4)]
  Trial2 <- Trial2[,c(-3,-4)]
  names(Trial1) = c("valve", "Subject", "Running.Block.", "trial1", "TMA.Concentration")
  names(Trial2) = c("valve", "Subject", "Running.Block.", "trial2", "TMA.Concentration")
  Trial.combo <- merge(Trial1, Trial2)#merge the dataframes
  testRetest.Exp3 <- ddply(Trial.combo, .variables = c("Subject"), function(x) trial.cor = cor(x$trial1, x$trial2))
  testRetest.Exp3$V1 <- abs(testRetest.Exp3$V1)#takes abs. value of correlation values
  return(testRetest.Exp3)
}

TrialSplit <- subset(Exp3retest.df, Trial <= 26)
Trial1 <- subset(TrialSplit, valve2 == 23)
Trial2 <- subset(TrialSplit, valve2 == 24)
TrialSplit2 <- subset(Exp3retest.df, Trial >26)
Trial3 <- subset(TrialSplit2, valve2 == 23)
Trial4 <- subset(TrialSplit2, valve2 == 24)

#make all the different combinations
TR.12 <- testRetest(Trial1, Trial2)
names(TR.12) <- c("Subject", "TR12")
TR.13 <- testRetest(Trial1, Trial3)
names(TR.13) <- c("Subject", "TR13")
TR.14 <- testRetest(Trial1, Trial4)
names(TR.14) <- c("Subject", "TR14")
TR.23 <- testRetest(Trial2, Trial3)
names(TR.23) <- c("Subject", "TR23")
TR.24 <- testRetest(Trial2, Trial4)
names(TR.24) <- c("Subject", "TR24")
TR.34 <- testRetest(Trial3, Trial4)
names(TR.34) <- c("Subject", "TR34")

TR.allCombos <- merge(TR.12, TR.13)
TR.allCombos <- merge(TR.allCombos, TR.14)
TR.allCombos <- merge(TR.allCombos, TR.23)
TR.allCombos <- merge(TR.allCombos, TR.24)
TR.allCombos <- merge(TR.allCombos, TR.34)

#melt the data
TR.allCombos.melt <- melt(TR.allCombos, c("Subject"))
#visualize
ggplot(TR.allCombos.melt, aes(x = factor(Subject), y = value, color = variable)) +
  geom_point() +
  ylim(0,1)


############################################################################################################

#Now try doing this by concentration as well
############################################################################################################

TrialSplit <- subset(Exp3retest.df, Trial <= 26)
Trial1 <- subset(TrialSplit, valve2 == 23)
Trial2 <- subset(TrialSplit, valve2 == 24)
TrialSplit2 <- subset(Exp3retest.df, Trial > 26)
Trial3 <- subset(TrialSplit2, valve2 == 23)
Trial4 <- subset(TrialSplit2, valve2 == 24)


Trial1 <- Trial1[,c(-3,-4)]
Trial2 <- Trial2[,c(-3,-4)]
Trial3 <- Trial3[,c(-3,-4)]
Trial4 <- Trial4[,c(-3,-4)]
names(Trial1) = c("valve", "Subject", "Running.Block.", "trial1", "TMA.Concentration")
names(Trial2) = c("valve", "Subject", "Running.Block.", "trial2", "TMA.Concentration")
names(Trial3) = c("valve", "Subject", "Running.Block.", "trial1", "TMA.Concentration")
names(Trial4) = c("valve", "Subject", "Running.Block.", "trial2", "TMA.Concentration")
#merge into dataframes with each combo in trials 1 and 2 so they can be compared.
Trial.12 <- merge(Trial1, Trial2)#merge the dataframes
Trial.34 <- merge(Trial3, Trial4)
names(Trial3) = c("valve", "Subject", "Running.Block.", "trial2", "TMA.Concentration")
Trial.13 <- merge(Trial1, Trial3)
Trial.14 <- merge(Trial1, Trial4)
names(Trial2) = c("valve", "Subject", "Running.Block.", "trial1", "TMA.Concentration")
Trial.23 <- merge(Trial2, Trial3)
Trial.24 <- merge(Trial2, Trial4)
Trial.combo <- rbind(Trial.12, Trial.34, Trial.13, Trial.14, Trial.23, Trial.24)

#find the correlation
testRetest.Exp3 <- ddply(Trial.combo, .variables = c("Subject", "TMA.Concentration"), function(x) trial.cor = cor(x$trial1, x$trial2))
testRetest.Exp3$V1 <- abs(testRetest.Exp3$V1)#takes abs. value of correlation values

#visualize
ggplot(testRetest.Exp3, aes(x = TMA.Concentration, y = V1, color = factor(Subject))) +
  geom_point() +
  ylim(0,1)


