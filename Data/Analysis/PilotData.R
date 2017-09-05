#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)

#Import Data
subj <- dir(path = "Raw Data/Pilot", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#name the trials based on valves
valve.list <- read.csv("Analysis/TMA_supra_Pilot_merge.csv")

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)
#lost some stuff, try graphing and get back to it. 

#graph
pdf("Analysis/pilotData.TMA.lowConcentrations.pdf", width = 8, height = 8)
ggplot(data = subset(df.trial_merge), aes(x = Concentration, y = IntensityRating.inv, color = Type)) +
  geom_point() +
  facet_wrap(~Type)
dev.off()



#graph each subject
for (i in c(3, 8)){
  plot <- ggplot(data = subset(df.trial_merge, Subject == i & Type == "TMA"), aes(x = Concentration, y = IntensityRating.inv, color = Type)) +
    geom_point() +
    facet_wrap(~Type) +
    ggtitle(paste("Subject # ", i))
  print(plot)
}

#what's going on with TMA?
ggplot(data = subset(df.trial_merge, Subject %in% c(5,7,8) & Type == "TMA"), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
    geom_point() +
    #facet_wrap(~Type) +
    ggtitle("TMA Response")

#graph each subject better
#pdf("pilot graphs.pdf")
# for (i in c(1, 3, 5, 6,7,8)){
#  plot <-  ggplot(data = subset(df.trial_merge, Subject == i), aes(x = Concentration, y = IntensityRating.inv, color = Type)) +
#     geom_point() +
#     ggtitle(paste("Subject # ", i))
#   print(plot)
# }
#dev.off()



########normalize data and graph it again ##########
#scale function
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}

subjMinMax <- ddply(df.trial_merge, .variables = c("Subject"), .fun = summarize, minValue = min(IntensityRating.inv), maxValue = max(IntensityRating.inv))
df.scale <- merge(df.trial_merge, subjMinMax)
df.scale$normValue <- scale01(df.scale$IntensityRating.inv, df.scale$minValue, df.scale$maxValue)


#graph normalized
#pdf("TMA SupraStyle Pilot1_normalized.pdf")
ggplot(data = subset(df.scale), aes(x = Concentration, y = normValue, color = Type)) +
  geom_point() +
  facet_wrap(~Type)
#dev.off()

#from this it looks like something is still going on with TMA, so we need to look at some other things. 
#Look at test-restest
#to get test-retest we are going to have to split the database into trials 1-9 and 10-18 for the first and second rating of the same stimulus. 
df.scale$Trial <- as.numeric(df.scale$Trial)
df1 <- subset(df.scale, Trial <= 40 & df.scale$Subject != 1)
df2 <- subset(df.scale, Trial >  40 & df.scale$Subject != 1)
df1$TrialRep <- 1
df2$TrialRep <- 2
#names(testDataRename1) <- c("RightType", "LeftType", "Subject", "Test", "IntensityRating", "trial1", "OdorType", "RightNostril", "LeftNostril")
#names(testDataRename2) <- c("RightType", "LeftType", "Subject", "Test", "IntensityRating", "trial2", "OdorType", "RightNostril", "LeftNostril")
#df1.sub <- subset(df1, select = c("Subject", "valve", "valve2", "IntensityRating.inv", "Type", "odor1", "odor2", "Concentration", "Concentration_fake", "TrialRep"))
#df2.sub <- subset(df2, select = c("Subject", "valve", "valve2", "IntensityRating.inv", "Type", "odor1", "odor2", "Concentration", "Concentration_fake", "TrialRep"))

df.trials <- merge(df1, df2, by = c("Subject", "valve", "valve2", "Type", "odor1", "odor2", "Concentration", "Concentration_fake"))
pdf("pilot1.testRetest.pdf", width = 11, height = 8)
ggplot(df.trials, aes(x = IntensityRating.inv.x, y = IntensityRating.inv.y, colour = factor(Subject))) +
  geom_point() +
  facet_wrap(~Type)
#dev.off()

#do it normalized
pdf("pilot1.testRetest_norm.pdf", width = 11, height = 8)
ggplot(df.trials, aes(x = normValue.x, y = normValue.y, colour = factor(Subject))) +
  geom_point() +
  facet_wrap(~Type)
#dev.off()

#correlation
testData.corr <- ddply(df.trials, .variables = c("Subject", "Type"), function(x) trial.cor = cor(x$IntensityRating.inv.x, x$IntensityRating.inv.y))
testData.corr$trial.corr <- abs(testData.corr$V1)
#fix from missing values when you have time
pdf("correlationBySubject.pdf", width = 11, height = 8)
ggplot(testData.corr, aes(x = Type, y = trial.corr, colour = Type))+ 
  geom_point() + 
  facet_wrap(~Subject) +
  geom_hline(yintercept= .5) +
  ylab("Pearson's Correlation") +
  ggtitle("Test-Retest Correlation per subject")
#dev.off()
#still need to do the correlation figure. 

#########DR######
#import
subj <- dir(path = "Raw Data/DR", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
DR  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
#subset variables
DR <- subset(DR, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
DR$IntensityRating.inv <- 580-as.integer(DR$IntensityRating)
#Trial Data
DR.trial <- subset(DR, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#name the trials based on valves
valve.list <- read.csv("Analysis/TMA_DR_merge.csv")
#merge with trial list
DR.trial_merge <- merge(DR.trial, valve.list)
DR.trial_merge$TMA.Concentration <- as.double(DR.trial_merge$TMA.Concentration) #make TMA.concentration into a number

#rename to compare with second run
DR1 <- DR.trial_merge


#plot
ggplot(data = subset(DR.trial_merge), aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(valve2))) +
  geom_point() +
  facet_wrap(~Subject) #+
  #scale_x_log10()

#code DEP and water as factors (-1 and -2, and exclude them from my model, but label them on the graph later)

#TODO:
#1. compare TMA from first to second run
#2. model these functions with equations
#3. fix the controls to graph well
#can we normalize and model this? are we actually getting a dose-response curve? 



############1. compare TMA from first to second run##############
#pull TMA only from the first set for the subjects who have run in both experiments
df.TMA <- subset(df.trial_merge, Type == "TMA" & Subject %in% c(unique(DR$Subject)))
#give TMA true concentrations from the first dataset
df.TMA$TMA.Concentration = 0
for(i in 1:length(df.TMA$valve)){
  if(df.TMA$Concentration[i] == 1){
    df.TMA$TMA.Concentration[i] = 0.1
  }
  if(df.TMA$Concentration[i] == 2){
    df.TMA$TMA.Concentration[i] = 0.3
  }
  if (df.TMA$Concentration[i] == 3){
    df.TMA$TMA.Concentration[i] = 0.6
  }
  if (df.TMA$Concentration[i] == 4){
    df.TMA$TMA.Concentration[i] = 1
  }
  if (df.TMA$Concentration[i] == 5){
    df.TMA$TMA.Concentration[i] = 3
  }
  if (df.TMA$Concentration[i] == 6){
    df.TMA$TMA.Concentration[i] = 6
  }
}
#give each an identifier
df.TMA$experiment = "Blocker"
DR.trial_merge$experiment = "TMA DR"
#subset df.TMA to match
df.TMA <- subset(df.TMA, select = c("valve", "Subject", "Trial", "valve2", "Running.Block.", "IntensityRating.inv", "TMA.Concentration", "experiment"))
#combine the two datasets
TMA.all <- rbind(df.TMA, DR.trial_merge)
#get rid of the blanks for now
TMA.all$TMA.Concentration = as.numeric(TMA.all$TMA.Concentration)

#graph
ggplot(data = TMA.all, aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(experiment))) +
  geom_point() +
  facet_grid(Subject~.)
#same graph with different wrapping
#pdf("TMA concentration data.pdf")
ggplot(data = TMA.all, aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(experiment))) +
  geom_point() +
  facet_wrap(~Subject)
#dev.off()

#graph just the matching concentrations of TMA
ggplot(data = subset(TMA.all, TMA.Concentration %in% c(.1,.3,.6,1,3,6)), aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(experiment))) +
  geom_point() +
  #facet_wrap(~Subject)
  facet_grid(Subject~.) +
  scale_x_log10()

#Look for effect of previous trial on the current trial. 


##############NEW DR Data - DR run 2##########
#import
subj <- dir(path = "Raw Data/DR-2", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
DR  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
#subset variables
DR <- subset(DR, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
DR$IntensityRating.inv <- 580-as.integer(DR$IntensityRating)
#Trial Data
DR.trial <- subset(DR, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#name the trials based on valves
valve.list <- read.csv("Analysis/TMA_DR_merge.csv")
#merge with trial list
DR.trial_merge <- merge(DR.trial, valve.list)

DR.trial_merge$TMA.Concentration <- as.double(DR.trial_merge$TMA.Concentration) #make TMA.concentration into a number

DR2 <- DR.trial_merge #rename to merge with other data


#plot
#pdf("TMADR_2.pdf")
ggplot(data = subset(DR.trial_merge), aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(valve2))) +
  geom_point() +
  facet_wrap(~Subject) 
#dev.off()

#look at both DRs together
DR2$Session <- 2
DR1$Session <- 1

DR_sessions <- rbind(DR1, DR2) #bind databases together
#graph
#pdf("Analysis/Pilot.TMAHigherConcentration_.pdf", height = 8, width = 8)
ggplot(data = DR_sessions, aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(Session))) +
  geom_point() +
  facet_wrap(~Subject, scales = "free") 
#dev.off()

####Jon ran it twice - compare his two runs####
#import
subj <- dir(path = "Raw Data/DR-2", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
DR  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
#subset variables
DR <- subset(DR, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating", "Session"))
DR$IntensityRating.inv <- 580-as.integer(DR$IntensityRating)
#Trial Data
DR.trial <- subset(DR, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv", "Session"))

#name the trials based on valves
valve.list <- read.csv("Analysis/TMA_DR_merge.csv")
#merge with trial list
DR.trial_merge <- merge(DR.trial, valve.list)
#take out blanks
DR.trial_merge$TMA.Concentration <- as.double(DR.trial_merge$TMA.Concentration) #make TMA.concentration into a number

#plot
#pdf("Subject5Compare.pdf")
ggplot(data = subset(DR.trial_merge, Subject == 5), aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(Session))) +
  geom_point() +
  facet_wrap(~Subject) 
#dev.off()

#Looks much better the second time - but says it just smells like ammonia. I probably need to run this myself as well. 
#its either because the lines needed to be "primed" or because the one way flow valve was messed up
#we may also consider getting rid of the plugs on the valves


##########adaptation########
#start with DR_trial_merge
#DR2 <- subset(DR.trial_merge, (Subject == 5 & Session == 3) | Subject == c(4,6,7,8)) #get rid of the extra run done by Jon
DR_test <- DR2   #change this line based on which Data you want to test
#DR_adapt <- DR.trial_merge
#DR_adapt$prev_conc = 0
k = 1
for(i in unique(DR_test$Subject)){
  DR_adapt = subset(DR_test, Subject == i)
  DR_adapt$prev_conc = 0
  for (j in 1:length(DR_adapt$Subject)){
    if (DR_adapt$Trial[j] != 1){
    prev_trial = DR_adapt$Trial[j] - 1
    DR_adapt$prev_conc[j] <- DR_adapt$TMA.Concentration[which(DR_adapt$Trial == prev_trial)]
    }
  }
  if(k == 1){
    DR_total = DR_adapt
    k= k+1
  }
  else{
  DR_total = rbind(DR_total, DR_adapt)
  k=k+1
  }
}
#check what's going on
test = subset(DR_total, prev_conc == .1) #
