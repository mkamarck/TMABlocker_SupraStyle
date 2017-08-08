#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
#library(drm)
library(drc)


#Import Data####
subj <- dir(path = "//monell/storage/mainland/Projects/TMA blocker/SupraStyle/Data/Raw Data/TMA_DR_jars", pattern="\\.txt$", full.names=TRUE) # creating a list of all file names
names(subj)  <- basename(subj)
df.jars <- ldply(subj, read.delim, stringsAsFactors=FALSE) 
df.jars <- subset(df.jars, select = c("Subject","Session", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating","Jar","Concentration")) 
df.jars$IntensityRating.inv <- 580-as.integer(df.jars$IntensityRating)


#Trial Data
df.trialJars <- subset(df.jars, Running.Block. == "Test", select = c("Subject", "Session", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv","Jar","Concentration"))
#write.csv(df.trial, "TMA_supra_9.csv") #give this to Dardalie to import


#Normalize####
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}
#returns dataframe with the minimum and maximum values for each subject
subjMinMax <- ddply(df.trialJars, .variables = c("Subject"), .fun = summarize, minValue = min(IntensityRating.inv), maxValue = max(IntensityRating.inv))
#merges the max and min values with the full dataframe
df.scaleJars <- merge(df.trialJars, subjMinMax)
#calculates the normalized values using the scale function as written
df.scaleJars$normValue <- scale01(df.scaleJars$IntensityRating.inv, df.scaleJars$minValue, df.scaleJars$maxValue)

#Plot all Subjects on diff graphs####

ggplot(data = df.scaleJars, aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Subject)

#Plot all Subjects on same  graph####
ggplot(data = df.scaleJars, aes(x = Concentration, y = normValue, color = factor(Subject))) +
  geom_point() 




#run HIll model#####

   #subset - imputs log10 of concentration column in small.scale into new conc.log column
df.scaleJars$concentration.log <- log10(df.scaleJars$Concentration)


TMAjars.model <- drm(formula = IntensityRating.inv~Concentration, data = df.scaleJars, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)    
 
TMAjars.graph = function(x){
  summary(TMAjars.model)$coefficients[2,1] + (summary(TMAjars.model)$coefficients[1,1]-summary(TMAjars.model)$coefficients[2,1])/(1+10^((log10(summary(TMAjars.model)$coefficients[3,1]) - x)*1))
}

   #drm is dose response model function , summary give sum. stats , 
   #LL.4 sets the 4 parameters of sigmoid curve ( slope at 1, looking for other)


#Below applies Hill model function to individual subjects#### 
ggplot(data = df.scaleJars, aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Subject)#+
  #stat_function(fun = TMAjars.graph)+
  #xlim(-2, 5)

#Hill model for normalized Intensity#### 
TMAjars.model <- drm(formula = normValue~Concentration, data = df.scaleJars, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)    

TMAjars.graph = function(x){
  summary(TMAjars.model)$coefficients[2,1] + (summary(TMAjars.model)$coefficients[1,1]-summary(TMAjars.model)$coefficients[2,1])/(1+10^((log10(summary(TMAjars.model)$coefficients[3,1]) - x)*1))
}

#Below gives graph w/ function applied to ALL subjects####

ggplot(data=df.scaleJars, aes(x = concentration.log, y = normValue)) +
  geom_point()+
  ggtitle(paste("All Subjects"))+
  stat_function(fun = TMAjars.graph)

#Below gives graph w/ function applied to specific subjects####
ggplot(data= subset(df.scaleJars, Subject == 40),aes(x = concentration.log, y = normValue)) +
  geom_point()+
  facet_wrap(~Subject)+
  stat_function(fun = TMAjars.graph)
  scale_x_log10()

 
#Does TMA smell stronger after mixing the bottle? See several trials on same graph####
png("figures/JarsTMAintensitySubj1000multiTrial.png", width = 360, height = 365)
ggplot(data = subset(df.trialJars, Subject == 1000& Session%in%c("1","2")), aes(x = Concentration, y = IntensityRating.inv, color = factor(Session))) +
 geom_point() +
  ggtitle(paste("Effect of Agitation on TMA Odor Intensity"))+
 facet_wrap(~Subject)+
 scale_x_log10()+
  xlab("Concentration (mM)") + 
  ylab("Intensity Rating")+
  scale_color_manual(labels = c("Before Agitation", "After Agitation"), values = c("blue", "red")) +
  theme_bw() +
  guides(color=guide_legend("Static Olfaction Sessions"))
dev.off()

#can we prove that TMA is rated lower before agitation? anova#########
aov.TMAJarsSes1Ses.2 <- aov(IntensityRating.inv~Session*Concentration, data = subset(df.trialJars, Session %in% c("1", "2")&Subject==1000))
summary(aov.TMAJarsSes1Ses.2 )

