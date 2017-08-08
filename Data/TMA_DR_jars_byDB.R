#DB script for looking at TMA dose response in Jars
#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(drm)



#Import Data####
subj <- dir(path = "Raw Data/TMA_DR_jars", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df.jars  <- ldply(subj, read.delim, stringsAsFactors=FALSE)

df.jars <- subset(df, select = c("Subject", "Trial", "TrainList", "Running.Block.", "IntensityRating","Jar","Concentration")) 
### df w/ columns of interest. this is where the other subjects are lost
df.jars$IntensityRating.inv <- 580-as.integer(df.jars$IntensityRating)


#Trial Data
df.trialJars <- subset(df.jars, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv","Jar","Concentration"))
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

#Plot all Subjects####

ggplot(data = subset(df.trialJars, Subject == 6), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) +
  geom_point() +
  facet_wrap(~Subject)

#for loop - plot each subject####
for (i in unique(df.scaleJars$Subject)) {
  plot = ggplot(data = subset(df.scaleJars, Subject == i), aes(x = Concentration, y = IntensityRating.inv, color = factor(Subject))) + 
    geom_point() +  # make points transparent (e.g. geom_point(alpha = 0.05)) or very small (e.g. geom_point(shape = ".")
    facet_wrap(~Type) +
    ggtitle(paste("Subject Number", i))   # ggtitle -> title of chart
  print(plot)
}


### For loop doesn't work





#Apply Sigmoid Function

df.jars.model<- subset(df.trialJars, Subject == 6 )
jars.data.model <- subset(df.jars.model, select = c("Concentration", "IntensityRating.inv")) 
# selects two columns of df.scale and makes a new df, data.model

# TMA sensitivity graph for subject 6
ggplot(df.jars.model, aes(x = Concentration, y = IntensityRating.inv)) +
  geom_point() +
  geom_smooth()

#run HIll model#####
#subset - imputs log10 of concentration column in small.scale into new conc.log column
df.jars.model$concentration.log <- log10(df.jars.model$Concentration)


model.TMAjars <- drm(formula = IntensityRating.inv~concentration.log, data = df.jars.model, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(model.TMAjars)    
TMAjars.graph = function(x){
  summary(model.TMAjars)$coefficients[2,1] + (summary(model.TMAjars)$coefficients[1,1]-summary(model.TMAjars)$coefficients[2,1])/(1+10^((log10(summary(model.TMAjars)$coefficients[3,1]) - x)*1))
}

#drm is dose response model function , summary give sum. stats , 
#LL.4 sets the 4 parameters of sigmoid curve ( slope at 1, looking for other)

ggplot(df.jars.model, aes(x = concentration.log, y = IntensityRating.inv)) +
  geom_point() +
  stat_function(fun = model.TMAjars)
### Finally, this last bit doesn't run. Get this: Error in layer(data = data, mapping = mapping, stat = StatFunction, 
#geom = geom,  : object 'model.TMAjars' not found