library(reshape2)
library(ggplot2)
library(plyr)
library(drc)


#Import Data####
subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/DR_Agitated", pattern="\\.txt$", full.names=TRUE) # creating a list of all file names
names(subj)  <- basename(subj)
df.jars.agit <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
df.jars.agit <- subset(df.jars.agit, select = c("Subject","Session", "SessionDate", "SessionStartDateTimeUtc", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating")) 
df.jars.agit$IntensityRating.inv <- 580-as.integer(df.jars.agit$IntensityRating)


#Trial Data
df.trialJars <- subset(df.jars.agit, Running.Block. == "Test", select = c("Subject", "Session", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv","SessionDate", "SessionStartDateTimeUtc"))
#write.csv(df.trial, "TMA_supra_9.csv") #give this to Dardalie to import


#import the file that has the trial types
valve.list <- read.csv("//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_DR_merge.csv")

#merge with trial list
df.trial_merge <- merge(df.trialJars, valve.list)

#For Test-Retest btwn trial 1 and 2######
Exp3retest1v2.df <- subset(df.trial_merge, Subject!=1000)

#Setup df that splits Trials -- 52 rounds , 13 sessions per trial
Exp3retestT1 <- subset(Exp3retest1v2.df,Trial<= 13)# df with Inten.Rate from  trial 1+2 
Exp3retestT2 <- subset(Exp3retest1v2.df,Trial>13 & Trial<=26)#df with Inten.Rate from  trial 3+4
Exp3retestT1 <- Exp3retestT1[,c(-1,-3,-4,-5)] #get rid of column w/ trial,valves and Session numbers
Exp3retestT2 <- Exp3retestT2[,c(-1,-3,-4,-5)] 
colnames(Exp3retestT1)[3] <- "trial1"
colnames(Exp3retestT2)[3] <- "trial2"
testExp3.trials <- merge(Exp3retestT1,Exp3retestT2)#merge the dataframes


#graph the trials
ggplot(data = subset(testExp3.trials), aes(x = trial1, y = trial2)) +
  geom_point() +
  facet_wrap(~Subject)

#Do the correlation
testExp3.corr <- ddply(testExp3.trials, .variables = c("Subject", "Concentration"), function(x) trial.cor = cor(x$trial1, x$trial2))
testExp3.corr$trial.corr <- abs(testExp3.corr$V1)#takes abs. value of correlation values


ggplot(testExp3.corr, aes(x = NA, y = trial.corr)) +
  geom_point() +
facet_wrap(~Subject)

###### Graph of Session&Time against correlation coeff for trials 1 and 2
testExp3.corr$SubjectOrder <- factor(testExp3.corr$Subject, levels = c(11, 50, 29, 33, 56, 49, 59, 58, 62, 54, 53), labels = c(1,2,3,4,5,6,7,8,9,10,11))
ggplot(testExp3.corr, aes(x = SubjectOrder, y = trial.corr)) +
  geom_point() #+
  #facet_wrap(~Subject)
#fit=lm(SessionStartDateTimeUtc~trial.corr,data=testExp3.corr) 


########## relabel to reorder input in column by rows 
  #datafram.df$column.name <- factor(levels=c"rowname.1","rowname.2""etc..."),labels=c("label for 1st level","etc...")




#For Test-Retest btwn trial 1 and 3######
Exp3retest1v3.df <- subset(df.trial_merge, Subject!=1000)

#Setup df that splits Trials -- 52 rounds , 13 sessions per trial####
E3.1v3.retestT1 <- subset(Exp3retest1v3.df,Trial<= 13)# df with Inten.Rate from  trial 1+2 
E3.1v3.retestT2 <- subset(Exp3retest1v3.df,Trial>26 & Trial<=39)#df with Inten.Rate from  trial 3+4
E3.1v3.retestT1 <- E3.1v3.retestT1[,c(-1,-3,-4,-5)] #get rid of column w/ trial,valves and Session numbers
E3.1v3.retestT2 <- E3.1v3.retestT2[,c(-1,-3,-4,-5)] 
colnames(E3.1v3.retestT1)[3] <- "trial1"
colnames(E3.1v3.retestT2)[3] <- "trial2"
test.1v3.E3.trials <- merge(E3.1v3.retestT1,E3.1v3.retestT2)#merge the dataframes



#graph the trials 1v3
ggplot(data = subset(test.1v3.E3.trials), aes(x = trial1, y = trial2)) +
  geom_point() +
  facet_wrap(~Subject)

#Do the correlation 1v4
testExp3.corr <- ddply(test.1v3.E3.trials, .variables = c("SessionStartDateTimeUtc","Subject"), function(x) trial.cor = cor(x$trial1, x$trial2))
testExp3.corr$trial.corr <- abs(testExp3.corr$V1)#takes abs. value of correlation values


ggplot(test.1v3.E3.trials, aes(x = NA, y = trial.corr)) +
  geom_point() +
  facet_wrap(~Subject)

###### Graph of Session&Time against correlation coeff for trials 1 and 2
ggplot(testExp3.corr, aes(x = SessionStartDateTimeUtc, y = trial.corr)) +
  geom_point() #+
#facet_wrap(~Subject)
#fit=lm(SessionStartDateTimeUtc~trial.corr,data=testExp3.corr) 

df.trial_merge$SubjectOrder <- factor(df.trial_merge$Subject, levels = c(11, 50, 29, 33, 56, 49, 59, 58, 62, 54, 53), labels = c(1,2,3,4,5,6,7,8,9,10,11))

ggplot(df.trial_merge, aes(x = TMA.Concentration, y = IntensityRating.inv, color = Subject))+
  geom_point()+
  facet_wrap(~SubjectOrder)













