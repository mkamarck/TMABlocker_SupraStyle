#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
library(plotrix)
#library(nls2)
library(drm)


small.scale <- subset(df.scale, Subject == 9 & Type == "Linalool")
data.model <- subset(small.scale, select = c("Concentration", "IntensityRating.inv")) 
# selects two columns of df.scale and makes a new df, data.model

#Linalool graph for subject 9
ggplot(small.scale, aes(x = Concentration, y = IntensityRating.inv)) +
  geom_point() +
  geom_smooth()

#run HIll model#####
#subset - imputs log10 of concentration column in small.scale into new conc.log column
small.scale$concentration.log <- log10(small.scale$Concentration)


model.linalool <- drm(formula = IntensityRating.inv~Concentration, data = small.scale, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(model.linalool)    
linalool.graph = function(x){
  summary(model.linalool)$coefficients[2,1] + (summary(model.linalool)$coefficients[1,1]-summary(model.linalool)$coefficients[2,1])/(1+10^((log10(summary(model.linalool)$coefficients[3,1]) - x)*1))
}

#drm is dose response model function , summary give sum. stats , 
#LL.4 sets the 4 parameters of sigmoid curve ( slope at 1, looking for other)
ggplot(small.scale, aes(x = concentration.log, y = IntensityRating.inv)) +
  geom_point() +
  stat_function(fun = linalool.graph)


