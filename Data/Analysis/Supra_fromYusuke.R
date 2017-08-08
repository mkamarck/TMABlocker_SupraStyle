#working with Yusuke's code to try to get the hill equation model to fit our data. 
########get some code to work with#######
#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)

#Import Data
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
#scale data
df.scale <- ddply(.data = df.trial_merge, .variables = c("Subject"), .fun = mutate, normIntensity = scale(IntensityRating.inv))


#graph
ggplot(df.trial_merge, aes(x = TMA.Concentration, y = IntensityRating.inv, colour = factor(valve2))) +
  geom_point() +
  facet_wrap(~Subject)
#load packages ##########
library("drc")
library("sandwich")
library("lmtest")
library("multcomp")
library("readr")
library("dplyr")
library("tibble")
library("tidyr")
library("purrr")
library("rlist")
library("stringr")
library("pipeR")
library("rvest")
library("ggplot2")
library("gridExtra")
library("rsvg")
library("naturalsort")

######yusuke's code - untouched#####
temp_data <- data %>>% filter(Odor %in% c("Clean air", "Paraffin oil", odor)) #isolates data from a single odor (from the 4 supra odors tested each week)
temp_conc <- temp_data %>>% (.$Concentration) %>>% sort() %>>% unique() #
min_conc <- min(temp_conc[!temp_conc %in% c(conc_Solvent, conc_Air)])
max_conc <- max(temp_conc)
form <- formula("IntensityRating ~ Concentration")
# Use fixed Top value calculated by the following equation
# Top = 17.14 * (log10VP[mmHg]+10)^3 / 500 + 14.79
VP = temp_data %>>% filter(Odor == odor) %>>% (.$VP) %>>% unique()
top = 17.14 * VP + 14.79
max_int = test %>>% 
  group_by(Concentration) %>>% 
  summarise(mean = mean(IntensityRating), sd = sd(IntensityRating)) %>>% 
  ungroup() %>>% 
  mutate(mean_sd = mean + sd) %>>% 
  arrange(desc(mean)) %>>% 
  (.[1, "mean_sd"]) %>>% 
  as.numeric()
top_upper = max(top, max_int)
temp_fit <- drm(form, 
                data = temp_data, 
                lowerl = c(-4*log(10), 0, 0, min_conc), 
                upperl = c(0, top_upper, top_upper, NA), 
                fct = L.4())


###old formula#########
TMAjars.model <- drm(formula = IntensityRating.inv~TMA.Concentration, 
                     data = subset(df.trial_merge, Subject ==11), 
                     fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(TMAjars.model)   
#Trying to modify ours to fit this...#######
form = formula("IntensityRating ~ Concentration")
TMA_fit <- drm(formula = form, 
               data = df.trial_merge, 
               lowerl = c(-4*log(10), 0, 0, min_conc),
               upperl = c(0,top_upper, top_upper, NA),
               fct = L.4())
summary(TMA_fit)    
TMAjars.graph = function(x){
  summary(TMA_fit)$coefficients[2,1] + (summary(TMA_fit)$coefficients[1,1]-summary(TMA_fit)$coefficients[2,1])/(1+10^((log10(summary(TMA_fit)$coefficients[3,1]) - x)*1))
}

for (i in unique(df.trial_merge$Subject)){
  ###modifying what yusuke has########
  temp_data <- subset(df.trial_merge, Subject == i & TMA.Concentration !=0)
  temp_conc <- temp_data %>>% (.$TMA.Concentration) %>>% sort() %>>% unique() #
  #min_conc <- min(temp_conc[!temp_conc %in% c(conc_Solvent, conc_Air)]) #takes the minimum concentration whichi s not the concentration of solvent or air
  min_conc <- min(temp_conc[temp_conc != 0])
  max_conc <- max(temp_conc)
  form <- formula("IntensityRating.inv ~ TMA.Concentration")
  
    # Use fixed Top value calculated by the following equation  --- MK: I'm interested where this equation comes from - it takes into account VP and somehow also the top of the scale
    # Top = 17.14 * (log10VP[mmHg]+10)^3 / 500 + 14.79
    #VP = temp_data %>>% filter(Odor == odor) %>>% (.$VP) %>>% unique() #sets VP for odor based on the dataframe
    #I'm going to look up VP for TMA
    #VP for TMA = 1487.83 mm Hg --- do I need to take the log 10 of it and then raise it to the third????
     VP = 1487.83
     VP2 = ((log10(VP)+ 10)^3) /500
     top = 17.14 * VP2 + 14.79
    #top = 500
    #he had test= data without controls included
    max_int = df.trial_merge %>>% 
      group_by(TMA.Concentration) %>>% 
      summarise(mean = mean(IntensityRating.inv), sd = sd(IntensityRating.inv)) %>>% 
      ungroup() %>>% 
      mutate(mean_sd = mean + sd) %>>% 
      arrange(desc(mean)) %>>% 
      (.[1, "mean_sd"]) %>>% 
      as.numeric()
  top_upper = max(top, max_int) #I'm not sure how to do this for fitting with multiple people, for now I'm just going to set the top to 500
  top_upper = 500
  temp_fit <- drm(form, 
                  data = temp_data, 
                  lowerl = c(-4*log(10), 0, 0, min_conc), 
                  upperl = c(0, top_upper, top_upper, NA), 
                  fct = LL.4())
  #using LL.4 instead of L.4 becuase the function actually seems to fit here!
  
  summary(temp_fit)
  #based on old data
  TMAjars.graph = function(x){
    summary(temp_fit)$coefficients[3,1] + (summary(temp_fit)$coefficients[2,1]-summary(temp_fit)$coefficients[3,1])/(1+10^((log10(summary(temp_fit)$coefficients[4,1]) - x)*summary(temp_fit)$coefficients[1,1]))
  } #This also had to be modified to include the slope command since its no longer fixed to 1, and all of the indexing also had to be modified
  
  temp_data$logConcentration <- log10(temp_data$TMA.Concentration)
 print(ggplot(temp_data, aes(x = logConcentration, y = IntensityRating.inv)) +
    geom_point() +
    stat_function(fun = TMAjars.graph) +
    ggtitle(paste("Subject # ", i))
 )
}

#This works! Now we just need to figure out how to do it for multiple subjects 
######Multiple Subjects###
df.scale <- ddply(.data = df.trial_merge, .variables = c("Subject"), .fun = mutate, normIntensity = scale(IntensityRating.inv))

ggplot(df.scale, aes(x = TMA.Concentration, y = normIntensity)) +
  geom_point() +
  facet_wrap(~Subject)

ggplot(df.scale, aes(x = TMA.Concentration, y = normIntensity)) +
  geom_point()
##Trying to fit multiple people now that the graph is normalized
temp_data <- subset(df.scale, TMA.Concentration !=0)
temp_conc <- temp_data %>>% (.$TMA.Concentration) %>>% sort() %>>% unique() #
#min_conc <- min(temp_conc[!temp_conc %in% c(conc_Solvent, conc_Air)]) #takes the minimum concentration whichi s not the concentration of solvent or air
min_conc <- min(temp_conc[temp_conc != 0])
max_conc <- max(temp_conc)
form <- formula("normIntensity ~ TMA.Concentration")

# # Use fixed Top value calculated by the following equation  --- MK: I'm interested where this equation comes from - it takes into account VP and somehow also the top of the scale
# # Top = 17.14 * (log10VP[mmHg]+10)^3 / 500 + 14.79
# #VP = temp_data %>>% filter(Odor == odor) %>>% (.$VP) %>>% unique() #sets VP for odor based on the dataframe
# #I'm going to look up VP for TMA
# #VP for TMA = 1487.83 mm Hg --- do I need to take the log 10 of it and then raise it to the third????
# VP = 1487.83
# VP2 = ((log10(VP)+ 10)^3) /500
# top = 17.14 * VP2 + 14.79
# #top = 500
#he had test= data without controls included
max_int = df.trial_merge %>>% 
  group_by(TMA.Concentration) %>>% 
  summarise(mean = mean(IntensityRating.inv), sd = sd(IntensityRating.inv)) %>>% 
  ungroup() %>>% 
  mutate(mean_sd = mean + sd) %>>% 
  arrange(desc(mean)) %>>% 
  (.[1, "mean_sd"]) %>>% 
  as.numeric()
#top_upper = max(top, max_int) #I'm not sure how to do this for fitting with multiple people, for now I'm just going to set the top to 500
top_upper = 5
temp_fit <- drm(form, 
                data = temp_data, 
                lowerl = c(-4*log(10), -2, -2, min_conc), #had to change the limits for the normalization process.
                upperl = c(0, top_upper, top_upper, NA), #why is slope upperlimit equal to zero? 
                fct = LL.4())
#using LL.4 instead of L.4 becuase the function actually seems to fit here!

summary(temp_fit)
#based on old data
TMAjars.graph = function(x){
  summary(temp_fit)$coefficients[3,1] + (summary(temp_fit)$coefficients[2,1]-summary(temp_fit)$coefficients[3,1])/(1+10^((log10(summary(temp_fit)$coefficients[4,1]) - x)*summary(temp_fit)$coefficients[1,1]))
}  
df.scale$Concentration <- log10(df.scale$TMA.Concentration)
ggplot(df.scale, aes(x = TMA.Concentration, y = normIntensity)) +
  geom_point() +
  geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
  geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1)

ggplot(new_data_for_plot, aes(x = Concentration, y = Prediction))+
  geom_line() +
  geom_ribbon(data = new_data_for_plot, aes(ymin = Lower, ymax = Upper), alpha = 0.1)


#we will have to test this to see if the people who have really bad test-retest improve this graph
conc_Air = 0
new_data_for_plot <- expand.grid(Concentration = seq(min_conc, max_conc, length.out = 1000))
# predictions and confidence intervals
pm <- predict(temp_fit, newdata = new_data_for_plot, interval = "confidence") %>>% data.frame()
new_data_for_plot <- bind_cols(new_data_for_plot, pm) 
#Also trying to get 95% confidence interval 
plot <- plot + 
  geom_line(data = new_data_for_plot) + 
  geom_ribbon(data = new_data_for_plot, aes(ymin = Lower, ymax = Upper, fill = Seq), alpha = 0.1)


## Comparing the dese-response model to a model where the response is on average constant and hence not changing with dose.
doseReponse <- as.numeric(noEffect(temp_fit)["p-value"]) < 0.05
## Top is higher than bottom?
new_data <- expand.grid(Concentration = seq(conc_Air, max_conc, length.out = 1000)) # range of concentrations
pred <- predict(temp_fit, newdata = new_data, interval = "confidence") %>>% data.frame()
TovB <- (max(pred$Lower, na.rm = TRUE) > min(pred$Upper, na.rm = TRUE) & doseReponse)
## Lack-of-fit test
pLoF <- modelFit(temp_fit) %>>% (.$`p value`[2])
## Mean absolute percent error
MAPE <- mape(temp_fit)


#working on final graph
summary_test <- df.scale %>>% 
  group_by(TMA.Concentration) %>>% 
  summarise(Intensity = mean(normIntensity), se = sd(normIntensity)/sqrt(length(normIntensity))) %>>% 
  ungroup()

geom_errorbar(data = summary_test, aes(ymin = Intensity - se, ymax = Intensity + se), width = 0.1) + # error bars of samples
  

ggplot(df.scale, aes(x = TMA.Concentration, y = normIntensity)) +
  geom_point() +
  geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
  geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
  geom_errorbar(data = summary_test, aes(x = Concentration, y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1)  # error bars of samples


###########################################################
#####Do the whole thing without normalizing the data#######
temp_data <- subset(df.scale, TMA.Concentration !=0 & !Subject %in% c(14, 16, 18, 36))

temp_conc <- temp_data %>>% (.$TMA.Concentration) %>>% sort() %>>% unique() #
#min_conc <- min(temp_conc[!temp_conc %in% c(conc_Solvent, conc_Air)]) #takes the minimum concentration whichi s not the concentration of solvent or air
min_conc <- min(temp_conc[temp_conc != 0])
max_conc <- max(temp_conc)
form <- formula("IntensityRating.inv ~ TMA.Concentration")

# max_int = df.scale %>>% 
#   group_by(TMA.Concentration) %>>% 
#   summarise(mean = mean(normIntensity), sd = sd(normIntensity)) %>>% 
#   ungroup() %>>% 
#   mutate(mean_sd = mean + sd) %>>% 
#   arrange(desc(mean)) %>>% 
#   (.[1, "mean_sd"]) %>>% 
#   as.numeric()
#top_upper = max(top, max_int) #I'm not sure how to do this for fitting with multiple people, for now I'm just going to set the top to 500
top_upper = 500
temp_fit <- drm(form, 
                data = temp_data, 
                lowerl = c(-4*log(10), 0, 0, min_conc), #had to change the limits for the normalization process.
                upperl = c(0, top_upper, top_upper, NA), #why is slope upperlimit equal to zero? 
                fct = LL.4())
#using LL.4 instead of L.4 becuase the function actually seems to fit here! - not sure the difference, maybe in inputting logarhythms or something
summary(temp_fit)

#make prediction dataframe
conc_Air = 0
new_data_for_plot <- expand.grid(Concentration = seq(min_conc, max_conc, length.out = 1000))
# predictions and confidence intervals
pm <- predict(temp_fit, newdata = new_data_for_plot, interval = "confidence") %>>% data.frame()
new_data_for_plot <- bind_cols(new_data_for_plot, pm) 
#find mean and SEM


summary_test <- df.scale %>>% 
  group_by(TMA.Concentration) %>>% 
  summarise(Intensity = mean(IntensityRating.inv), se = sd(IntensityRating.inv)/sqrt(length(IntensityRating.inv))) %>>% 
  ungroup()

#plot
png("Analysis/Figures/Experiment3_rawData_curve_withScale.png")
ggplot(df.scale, aes(x = TMA.Concentration)) +
  #geom_point() +
  geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
  geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
  geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) + # error bars of samples
  #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
  scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + #yusuke's line *5 +
  scale_x_log10()
dev.off()
#with the scale continuous, it does not look very good, but looks better at the y; would need to fix the clean, 0,  values to make this graph work

png("Analysis/Figures/Experiment3_rawData_curve.png")
ggplot(df.scale, aes(x = TMA.Concentration)) +
  #geom_point() +
  geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
  geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
  geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) #+ # error bars of samples
#   #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
#   scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + #yusuke's line *5 +
#   scale_x_log10()
dev.off()


##################################################################################

####Do it with normalized data and removing the subjects who are shitty at the task

#################################################################################
temp_data <- subset(df.scale, TMA.Concentration !=0)
temp_conc <- temp_data %>>% (.$TMA.Concentration) %>>% sort() %>>% unique() #
#min_conc <- min(temp_conc[!temp_conc %in% c(conc_Solvent, conc_Air)]) #takes the minimum concentration whichi s not the concentration of solvent or air
min_conc <- min(temp_conc[temp_conc != 0])
max_conc <- max(temp_conc)
form <- formula("normIntensity ~ TMA.Concentration")
  


#top_upper = max(top, max_int) #I'm not sure how to do this for fitting with multiple people, for now I'm just going to set the top to 500
top_upper = 5
temp_fit <- drm(form, 
                data = temp_data, 
                lowerl = c(-4*log(10), -5, -5, min_conc), #had to change the limits for the normalization process.
                upperl = c(0, top_upper, top_upper, NA), #why is slope upperlimit equal to zero? 
                fct = LL.4())
#using LL.4 instead of L.4 becuase the function actually seems to fit here! - not sure the difference, maybe in inputting logarhythms or something
summary(temp_fit)

#make prediction dataframe
conc_Air = 0
new_data_for_plot <- expand.grid(Concentration = seq(min_conc, max_conc, length.out = 1000))
# predictions and confidence intervals
pm <- predict(temp_fit, newdata = new_data_for_plot, interval = "confidence") %>>% data.frame()
new_data_for_plot <- bind_cols(new_data_for_plot, pm) 

#find mean and SEM
# summary_test <- df.scale %>>% 
#   group_by(TMA.Concentration) %>>% 
#   summarise(Intensity = mean(normIntensity), se = sd(normIntensity)/sqrt(length(normIntensity))) %>>% 
#   ungroup()

summary_test <- ddply(df.scale, .variables = c("TMA.Concentration"), .fun = summarize, Intensity = mean(normIntensity), se = sd(normIntensity)/sqrt(length(normIntensity)))

#plot
png("Analysis/Figures/Experiment3_normalized_curve.png")
ggplot(df.scale, aes(x = TMA.Concentration, y = normIntensity)) +
  #geom_point() +
  geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
  geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
  geom_errorbar(data = summary_test, aes(x = Concentration, y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1)  # error bars of samples
dev.off()
