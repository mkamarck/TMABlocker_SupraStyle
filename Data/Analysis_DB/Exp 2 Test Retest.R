library(reshape2)
library(ggplot2)
library(plyr)
library(drc)

subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/TMA_DR_jars", pattern="\\.txt$", full.names=TRUE) # creating a list of all file names
names(subj)  <- basename(subj)
df.jars <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
df.jars <- subset(df.jars, select = c("Subject","Session", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating","Jar","Concentration")) 
df.jars$IntensityRating.inv <- 580-as.integer(df.jars$IntensityRating)


#Trial Data
df.trialJars <- subset(df.jars, Running.Block. == "Test", select = c("Subject", "Session", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv","Jar","Concentration"))
#write.csv(df.trial, "TMA_supra_9.csv") #give this to Dardalie to import

#For Test-Retest######
Exp2retest.df <- df.trialJars
#Setup df that splits and renames IntensityRating as Trial1/Trial2
Exp2retestT1 <- subset(Exp2retest.df,Subject==1000&Session==1)# df with Inten.Rate from  trial 1 
Exp2retestT2 <- subset(Exp2retest.df,Subject==1000&Session==2)#df with Inten.Rate from  trial 2
Exp2retestT1 <- Exp2retestT1[,c(-2,-3,-8)] #get rid of column w/ trial and Session numbers
Exp2retestT2 <- Exp2retestT2[,c(-2,-3,-8)] 
colnames(Exp2retestT1)[5] <- "trial1"
colnames(Exp2retestT2)[5] <- "trial2"
testExp2.trials <- merge(Exp2retestT1,Exp2retestT2)#merge the dataframes

#Run pearsons correlation test####
testExp2.corr <- ddply(testExp2.trials, .variables = "Subject", function(x) trial.cor = cor(x$trial1, x$trial2))
testExp2.corr$trial.corr <- abs(testExp2.corr$V1)#takes abs. value of correlation values

ggplot(testExp2.corr, aes(x = Concentration, y = trial.corr)) +
  geom_point() +
  facet_wrap(~Subject)


