#Experiment 1 Graphs with curves

#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
library(drc)
library(sandwich)
library(lmtest)
library(multcomp)
library(readr)
library(tibble)
library(tidyr)
library(purrr)
library(rlist)
library(stringr)
library(pipeR)
library(rvest)
library(ggplot2)
library(gridExtra)
library(rsvg)
library(naturalsort)

#Import Data
subj <- dir(path = "/Volumes/mainland/Projects/TMA\ blocker/SupraStyle/Data/Raw\ Data/Test1_olfactometer", pattern="\\.txt$", full.names=TRUE)# on mac - creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE) 

#subset variables
df <- subset(df, select = c("Subject", "Trial", "valve", "valve2", "TrainList", "Running.Block.", "IntensityRating"))
df$IntensityRating.inv <- 580-as.integer(df$IntensityRating)

#Trial Data
df.trial <- subset(df, Running.Block. == "Test", select = c("Subject", "Trial", "valve", "valve2", "Running.Block.", "IntensityRating.inv"))

#import the file that has the trial types
valve.list <- read.csv("/Volumes/mainland/Projects/TMA blocker/SupraStyle/Data/Analysis/TMA_supra_highTMA_merge.csv") #for a mac

#merge with trial list
df.trial_merge <- merge(df.trial, valve.list)

#visualize data
ggplot(subset(df.trial_merge), aes(x = Concentration, y = IntensityRating.inv, color = Type)) +
  geom_point() +
  facet_wrap(~Type)

###############################Make all clean jars have same label###################
df.trial_merge$Type_new <- as.factor(df.trial_merge$Type)
for (i in 1:length(df.trial_merge$Type)){
  if(df.trial_merge$Type[i] %in% c("Blank 1", "Blank 2", "blank combo", "Blank combo")){
    df.trial_merge$Type_new[i] <- "Blank 1"
  }  
#   else{
#     df.trial_merge$Type_new[i] <- df.trial_merge$Type[i]
#   }
}

#scale data
df.scale <- ddply(.data = df.trial_merge, .variables = c("Subject"), .fun = mutate, normIntensity = scale(IntensityRating.inv))


################################################################

##################write the hill function - outputs the predicted values and stuff you need to make the graph#####

################################################################
HillEquationFit <- function(temp_data, form, normalized = FALSE){
  temp_conc <- temp_data %>>% (.$Concentration) %>>% sort() %>>% unique() #
  min_conc <- min(temp_conc[temp_conc != 0])
  max_conc <- max(temp_conc)
  if(!normalized){
    top_upper = 500
    low_bottom = 0
  }
  else{
    top_upper = 10
    low_bottom = -5
  }
  
  temp_fit <- drm(form, 
                  data = temp_data, 
                  lowerl = c(-4*log(10), low_bottom, low_bottom, min_conc), #had to change the limits for the normalization process.
                  upperl = c(0, top_upper, top_upper, NA), #why is slope upperlimit equal to zero? 
                  fct = LL.4())
  #using LL.4 instead of L.4 becuase the function actually seems to fit here! - not sure the difference, maybe in inputting logarhythms or something
  print(summary(temp_fit))
  #make prediction dataframe
  new_data_for_plot <- expand.grid(Concentration = seq(min_conc, max_conc, length.out = 1000))
  # predictions and confidence intervals
  pm <- predict(temp_fit, newdata = new_data_for_plot, interval = "confidence") %>>% data.frame()
  ##new_data_for_plot <- bind_cols(new_data_for_plot, pm) 
  new_data_for_plot <- cbind(new_data_for_plot, pm)
  #find mean and SEM
  return(new_data_for_plot)
}


################################################################

#Fit curves to all graphs RAW DATA

################################################################
#for the first pass, this is going to be done without clean jars because I'm not sure how to handle them yet. 

#define these things for the function
pdf("Analysis/Figures/Analysis_experiment1_withCurves.pdf")
for (i in unique(df.trial_merge$Type_new)){
  if(i != "Blank 1"){
    temp_data <- subset(df.trial_merge, Concentration !=0 & !Subject %in% c(14, 16, 18, 36) & Type == i)
    form <- formula("IntensityRating.inv ~ Concentration")
    print(paste("Summary of Type = ", i))
    new_data_for_plot <- HillEquationFit(temp_data, form, normalized = FALSE)
    
    #test function plot
    print(ggplot(temp_data, aes(x = Concentration, y = IntensityRating.inv)) +
      geom_point() +
      geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
      geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
      ggtitle(paste("Test Graph for Type = ", i))
    )
    #get mean and SEM
    summary_test <- ddply(temp_data, .variables = c("Concentration"), .fun = summarize, Intensity = mean(IntensityRating.inv), se = sd(IntensityRating.inv)/sqrt(length(IntensityRating.inv)))
    
    #plot
    #png("Analysis/Figures/Experiment3_rawData_curve_withScale.png")
    print(ggplot(df.scale, aes(x = Concentration)) +
      #geom_point() +
      geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
      geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
      geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) + # error bars of samples
      #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
      scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) +#yusuke's line *5 
      ggtitle(paste("Type = ", i))  
        ) 
       #dev.off()
  #looks weird with log scale because the concentrations for linalool aren't real in this dataframe (just labled 1-6 to make life easier. How much does this affect the graph???)
  }
  else{
    print("Blank 1")
  }
}
#nonenol + Linalool looks really weird. 
dev.off()

# #This code is leftover for individual graphs
# ggplot(df.scale, aes(x = Concentration)) +
#   #geom_point() +
#   geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
#   geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
#   geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) #+ # error bars of samples
# #   #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
# #   scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + #yusuke's line *5 +
# #   scale_x_log10()

################################################################

#Fit curves to all graphs NORMALIZED DATA

################################################################
pdf("Analysis/Figures/Analysis_experiment1_withCurves_normalized_linaloolandTMA.pdf")
df.scale_sub <- subset(df.scale, Type %in% c("Linalool", "TMA"))
for (i in unique(df.scale_sub$Type_new)){
  if(i != "Blank 1"){
    temp_data <- subset(df.scale_sub, Concentration !=0 & !Subject %in% c(14, 16, 18, 36) & Type == i)
    form <- formula("normIntensity ~ Concentration")
    print(paste("Summary of Type = ", i))
    new_data_for_plot <- HillEquationFit(temp_data, form, normalized = TRUE)
    
    #test function plot
#     print(ggplot(temp_data, aes(x = Concentration, y = normIntensity)) +
#             geom_point() +
#             geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
#             geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
#             ggtitle(paste("Test Graph for Type = ", i))
#     )
    #get mean and SEM
    summary_test <- ddply(temp_data, .variables = c("Concentration"), .fun = summarize, Intensity = mean(normIntensity), se = sd(normIntensity)/sqrt(length(normIntensity)))
    
    #plot
    #png("Analysis/Figures/Experiment3_rawData_curve_withScale.png")
    print(ggplot(summary_test, aes(x = Concentration, y = Intensity)) +
            geom_point() +
            geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
            geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
            geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) + # error bars of samples
            #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
            #scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) +#yusuke's line *5 
            ggtitle(paste("Type = ", i))  +
            theme_bw() +
            ylim(-1.5, 2.5)
    ) 
    #dev.off()
    #looks weird with log scale because the concentrations for linalool aren't real in this dataframe (just labled 1-6 to make life easier. How much does this affect the graph???)
  }
  else{
    print("Blank 1")
  }
}
dev.off()

################################################################

#Fit curves to RAW TMA data

################################################################

temp_data <- subset(df.trial_merge, Concentration !=0 & !Subject %in% c(14, 16, 18, 36) & Type == TMA)
form <- formula("IntensityRating.inv ~ Concentration")
print(paste("Summary of Type = ", i))
new_data_for_plot <- HillEquationFit(temp_data, form, normalized = FALSE)

#test function plot
print(ggplot(temp_data, aes(x = Concentration, y = IntensityRating.inv)) +
        geom_point() +
        geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
        geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
        ggtitle(paste("Test Graph for Type TMA"))
)
#get mean and SEM
summary_test <- ddply(temp_data, .variables = c("Concentration"), .fun = summarize, Intensity = mean(IntensityRating.inv), se = sd(IntensityRating.inv)/sqrt(length(IntensityRating.inv)))

#plot
#png("Analysis/Figures/Experiment3_rawData_curve_withScale.png")
print(ggplot(df.scale, aes(x = Concentration)) +
        #geom_point() +
        geom_line(data = new_data_for_plot, aes(x = Concentration, y = Prediction)) +
        geom_ribbon(data = new_data_for_plot, aes(x = Concentration, y = Prediction, ymin = Lower, ymax = Upper), alpha = 0.1) +
        geom_errorbar(data = summary_test, aes(y = Intensity, ymin = Intensity - se, ymax = Intensity + se), width = 0.1) + # error bars of samples
        #scale_y_continuous(limits = c(0, 100), breaks = c(0, 1.4, 6.1, 17.2, 35.4, 53.3, 100), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) + 
        scale_y_continuous(limits = c(0, 500), breaks = c(0, 7, 30.5, 17.2*5, 35.4*5, 53.3*5, 100*5), labels = c("", "Barely detectable", "Weak", "Moderate", "Strong", "Very strong", "Strongest imaginable \nsensation of any kind")) +#yusuke's line *5 
        ggtitle(paste("Type = ", i))  
) 
#dev.off()
#looks weird with log scale because the concentrations for linalool aren't real in this dataframe (just labled 1-6 to make life easier. How much does this affect the graph???)

