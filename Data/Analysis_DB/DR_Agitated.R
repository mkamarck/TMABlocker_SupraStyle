#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
#library(drm)
library(drc)


#Import Data####
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


#Normalize####
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}
#returns dataframe with the minimum and maximum values for each subject
subjMinMax <- ddply(df.trial_merge, .variables = c("Subject"), .fun = summarize, minValue = min(IntensityRating.inv), maxValue = max(IntensityRating.inv))
#merges the max and min values with the full dataframe
df.scaleJars <- merge(df.trial_merge, subjMinMax)
#calculates the normalized values using the scale function as written
df.scaleJars$normValue <- scale01(df.scaleJars$IntensityRating.inv, df.scaleJars$minValue, df.scaleJars$maxValue)

#Plot all Subjects on diff graphs####

ggplot(data = df.scaleJars, aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Subject)#+
#stat_function(fun = TMAjars.graph)+
#xlim(-2, 5)

#scale using scale function
df.scaleJars <- ddply(df.trial_merge, .variables = c("Subject"), .fun = transform, normValue = scale(IntensityRating.inv))


#Plot all Subjects on same  graph####
ggplot(data = df.scaleJars, aes(x = TMA.Concentration, y = normValue, color = factor(Subject))) +
  geom_point() 



#try gettng rid of blank 
#df.scaleJars <- subset(df.scaleJars, TMA.Concentration !=0)

#Hill Model applied to Individuals####

#subset - imputs log10 of concentration column in small.scale into new conc.log column
df.scaleJars$concentration.log <- log10(df.scaleJars$TMA.Concentration)


TMAjars.model <- drm(formula = IntensityRating.inv~TMA.Concentration, data = df.scaleJars, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)    

TMAjars.graph = function(x){
  summary(TMAjars.model)$coefficients[2,1] + (summary(TMAjars.model)$coefficients[1,1]-summary(TMAjars.model)$coefficients[2,1])/(1+10^((log10(summary(TMAjars.model)$coefficients[3,1]) - x)*1))
}

      #drm is dose response model function , summary give sum. stats , 
      #LL.4 sets the 4 parameters of sigmoid curve ( slope at 1, looking for other)

#Trying the model for individual subjects
subj <- 33
TMAjars.model <- drm(formula = IntensityRating.inv~TMA.Concentration, data = subset(df.scaleJars, Subject ==subj), fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)    

TMAjars.graph = function(x){
  summary(TMAjars.model)$coefficients[2,1] + (summary(TMAjars.model)$coefficients[1,1]-summary(TMAjars.model)$coefficients[2,1])/(1+10^((log10(summary(TMAjars.model)$coefficients[3,1]) - x)*1))
}


#Below gives graph w/ function applied to specific subjects####
ggplot(data= subset(df.scaleJars, Subject == subj),aes(x = concentration.log, y = IntensityRating.inv)) +
  geom_point()+
  facet_wrap(~Subject)+
  stat_function(fun = TMAjars.graph) +
  xlim(-2,5)
  #scale_x_log10()


#Hill model for normalized Intensity#### 

df.scaleJars$concentration.log <- log10(df.scaleJars$TMA.Concentration)

TMAjars.model <- drm(formula = normValue~TMA.Concentration, data = df.scaleJars, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)    

TMAjars.graph = function(x){
  summary(TMAjars.model)$coefficients[2,1] + (summary(TMAjars.model)$coefficients[1,1]-summary(TMAjars.model)$coefficients[2,1])/(1+10^((log10(summary(TMAjars.model)$coefficients[3,1]) - x)*1))
}

#Below gives graph w/ function applied to ALL subjects####

ggplot(data=df.scaleJars, aes(x = concentration.log, y = normValue)) +
  geom_point()+
  ggtitle(paste("All Subjects"))+
  stat_function(fun = TMAjars.graph)



# See several trials on same graph####
ggplot(data = subset(df.trialJars, Subject == 11), aes(x = TMA.Concentration, y = IntensityRating.inv, color = factor(Session))) +
  geom_point() +
  facet_wrap(~Subject)+
  scale_x_log10()




