#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
library(drc)


#Import Data
subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
#subj <- dir(path = "/Volumes/mainland/Projects/TMA\ blocker/SupraStyle/Data/Raw\ Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# on mac - creating a list of all file names

names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 

#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#import the file that has the trial types
valve.list <- read.csv("//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a pc
#valve.list <- read.csv("/Volumes/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a mac

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)


# ggplot(subset(df.trial_merge, Subject == 8), aes(x = Concentration, y = IntensityRating.inv, colour = Type))+
#   geom_point() +
#   facet_wrap(~Type)


#TMAretest.df <- subset(df.trial_merge, Subject=5 & Type=="TMA") # need & when connecting booleans
# TMAtest.df <- df.trial_merge[df.trial_merge$Subject==5,]
# TMAretest.df <- TMAtest.df[TMAtest.df$Type=="TMA",]

#For Test-Retest######
TMAretest.df <- df.trial_merge

#Setup df that splits and renames IntensityRating as Trial1/Trial2

TMAretestRename1 <- subset(TMAretest.df,Trial<= 40)# df with Inten.Rate from  trial 1 
TMAretestRename2 <- subset(TMAretest.df,Trial> 40)#df with Inten.Rate from  trial 2
TMAretestRename1 <- TMAretestRename1[,-4] #get rid of trial numbers
TMAretestRename2 <- TMAretestRename2[,-4] #get rid of trial numbers
colnames(TMAretestRename1)[5] <- "Trial1"  # rename Intensity column "trial1"
colnames(TMAretestRename2)[5] <- "Trial2"
testTMA.trials <- merge(TMAretestRename1,TMAretestRename2)#merge the dataframes

#graph the trials
ggplot(data = subset(testTMA.trials), aes(x = Trial1, y = Trial2, color = Type)) +
  geom_point() +
  facet_wrap(~Subject)

#Do the correlation
testTMA.corr <- ddply(testTMA.trials, .variables = c("Subject", "Type"), function(x) trial.cor = cor(x$Trial1, x$Trial2))
testTMA.corr$trial.corr <- abs(testTMA.corr$V1)#takes abs. value of correlation values

ggplot(testTMA.corr, aes(x = Type, y = trial.corr)) +
  geom_point() +
  facet_wrap(~Subject)


ggplot(testTMA.corr, aes(x = Type, y = trial.corr, color = factor(Subject))) +
  geom_point() 

ggplot(subset(testTMA.corr, Type == "TMA"), aes(x = Subject, y = trial.corr))+
  geom_point() +
  ylim(0,1)

testRetest.Exp1 <- subset(testTMA.corr, Type == "TMA")
testRetest.Exp1$Experiment <- 1

#one option for comparing TMA to Linalool in terms of consistency#### Paired t-test 
png("figures/allSubjectsTMAlwrCorrLinalool.png",width = 200, height = 360,units = "px")
ggplot(subset(testTMA.corr, Type %in% c("TMA", "Linalool") & !Subject %in% c("14","16","18","36")), aes(x = Type, y = trial.corr, color = factor(Subject), group = Subject)) +
  geom_point() +
  geom_line()+
  ggtitle("Test-Retest Correlation") +
  xlab("") + 
  ylab("Intensity Rating Correlation Coefficient")+
  theme(legend.position = "none")
  #theme_bw() 
dev.off()

#Another option for comparing TMA to Linalool in terms of consistency####  
ggplot(subset(testTMA.corr, Type %in% c("TMA", "Linalool") & !Subject %in% c("14","16","18","36")), aes(x = Type, y = trial.corr, color = factor(Subject), group = Subject)) +
  geom_point() +
  geom_abline(a = 0, y = 1)+
  xlim(0,1) +
  ylim(0,1)+
  ggtitle("Test-Retest Correlation") +
  xlab("Linalool test-retest") + 
  ylab(" TMA test-retest")+
  theme(legend.position = "none")



#Paired t-test#####
statsCorrelation <- t.test(trial.corr~Type, data = subset(testTMA.corr, Type %in% c("TMA", "Linalool") & !Subject %in% c("14","16","18","36")), paired = TRUE)
statsCorrelation

#can we prove that TMA is rated lower than linalool? anova#########
aov.TMAvLinalool <- aov(IntensityRating.inv~Type*Concentration, data = subset(df.trial_merge, Type %in% c("TMA", "Linalool") & !Subject %in% c("14","16","18","36")))
summary(aov.TMAvLinalool)





########broken code ######
slope.correl.graphs <- matrix(1:3)
for (i in 1:3){
  temp <- subset(testTMA.trials,Subject==i)
  fit=lm(Trial1~Trial2,data=testTMA.trials)  #fits linear model to data
  slope.correl.graphs[i] <- fit$coefficients[2]
}






