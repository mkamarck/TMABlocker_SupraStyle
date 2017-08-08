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

#For Test-Retest btwn trial 1 and 2######
Exp3retest.df <- subset(df.trial_merge, Subject!=1000)

#Setup df that splits Trials -- 52 rounds , 13 sessions per trial
Exp3retestT1 <- subset(Exp3retest.df,Trial<= 13)# df with Inten.Rate from  trial 1
Exp3retestT2 <- subset(Exp3retest.df,Trial>13 & Trial<=26)#df with Inten.Rate from  trial 2
Exp3retestT1 <- Exp3retestT1[,c(-1,-3,-4,-5)] #get rid of column w/ trial and Session numbers
Exp3retestT2 <- Exp3retestT2[,c(-1,-3,-4,-5)] 
colnames(Exp3retestT1)[3] <- "trial1"
colnames(Exp3retestT2)[3] <- "trial2"


OnevTwoExp3.trials <- merge(Exp3retestT1,Exp3retestT2)#merge the dataframes for T1 and T2
colnames(OnevTwoExp3.trials)[3] <- "Concentration"  #renames column 8 to conc...



#Do the correlation
testExp3.corr <- ddply(OnevTwoExp3.trials, .variables = "Subject", function(x) trial.cor = cor(x$trial1, x$trial2))
testExp3.corr$trial.corr <- abs(testExp3.corr$V1)#takes abs. value of correlation values







#Import Data Exp1###
subject <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
#subj <- dir(path = "/Volumes/mainland/Projects/TMA\ blocker/SupraStyle/Data/Raw\ Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# on mac - creating a list of all file names

names(subject)  <- basename(subject)
df  <- ldply(subject, read.delim, stringsAsFactors=FALSE) 

#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#import the file that has the trial types
valve.list.exp1 <- read.csv("//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a pc
#valve.list <- read.csv("/Volumes/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a mac

#merge with trial list
df.trial_merge.1 <- merge(df.trial, valve.list.exp1)

Exp1.TMAretest.df <- df.trial_merge.1
Exp1.TMAretest.df <- Exp1.TMAretest.df[Exp1.TMAretest.df$Type=="TMA",]#subset w/ only TMA data
Exp1.TMAretest.df <- subset(Exp1.TMAretest.df, !Subject %in% c("14","16","18","36")) # Removes thes subjects, make p-value higher
#Setup df that splits and renames IntensityRating as Trial1/Trial2

exp1.TMAretestRename1 <- subset(Exp1.TMAretest.df,Trial<= 40)# df with Inten.Rate from  trial 1 
exp1.TMAretestRename2 <- subset(Exp1.TMAretest.df,Trial> 40)#df with Inten.Rate from  trial 2
exp1.TMAretestRename1 <- exp1.TMAretestRename1[,-4] #get rid of trial numbers
exp1.TMAretestRename2 <- exp1.TMAretestRename2[,-4] #get rid of trial numbers
colnames(exp1.TMAretestRename1)[5] <- "Trial1"  # rename Intensity column "trial1"
colnames(exp1.TMAretestRename2)[5] <- "Trial2"
exp1.testTMA.trials <- merge(exp1.TMAretestRename1,exp1.TMAretestRename2)#merge the dataframes

#Do the correlation
exp1.testTMA.corr <- ddply(exp1.testTMA.trials, .variables = c("Subject", "Type"), function(x) trial.cor = cor(x$Trial1, x$Trial2))
exp1.testTMA.corr$trial.corr <- abs(exp1.testTMA.corr$V1)#takes abs. value of correlation values







#Correlaton by Exp #####
exp1.testTMA.corr <- exp1.testTMA.corr[,-2] #Deletes column 2 from df

exp1.testTMA.corr$exp <- 1 #Creates new column to ID values from Exp.1
testExp3.corr$exp <- 3    #Creates new column to ID values from Exp.3


retest.exp1v3 <- rbind(testExp3.corr,exp1.testTMA.corr) #combines dataframes by column


#Do the correlation

statsCorrelation.exp1Vexp3 <- t.test(trial.corr~exp, data = subset(retest.exp1v3), paired = F)
statsCorrelation.exp1Vexp3

ggplot(retest.exp1v3, aes(x=exp, y = trial.corr))+
  geom_point()

######################################

#Marissa is comparing testRetest.Exp1 to testRetest.Exp3

#####################################
testRetest.Exp3$Type <- "TMA" #make row names match
testRetest.1and3 <- rbind(testRetest.Exp1, testRetest.Exp3) #combine

test <- t.test(trial.corr~Experiment, data = testRetest.1and3)
test

ggplot(testRetest.1and3, aes(x = factor(Experiment), y = trial.corr)) +
  geom_point() +
  ylim(0,1)
