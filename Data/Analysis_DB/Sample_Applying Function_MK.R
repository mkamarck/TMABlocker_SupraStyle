#Import Libraries####
library(reshape2)
library(ggplot2)
library(plyr)
library(plotrix)
# library(nls2)

####practice with glm#####
small.scale <- df.scale[(1:80),]
#Look at subject 9
small.scale <- subset(df.scale, Subject == 9 & Type == "Linalool")
data.model <- subset(small.scale, select = c("Concentration", "IntensityRating.inv"))

# norm.values <- as.data.frame(small.scale$normValue)
# conc <- as.data.frame(small.scale$Concentration)
# norm.Intes.val <- as.double(unlist(norm.values))
# Conc.list<- as.numeric(unlist(conc))
# dat=as.data.frame(cbind(Conc.list,norm.Intes.val)) 

plot(Conc.list,norm.Intes.val,xlab="conc",ylab="normintensity")
g <- glm(norm.Intes.val~Conc.list,family=quasibinomial,dat)
curve(predict(g,data.frame(Conc.list=x),type="resp"),add=TRUE)
# points(Conc.list,fitted(g),pch=20) - OPTIONAL, adds points to curve


#Mk rewrite plot command
model.glm <- glm(IntensityRating.inv~Concentration, data = small.scale)
summary(model.glm)  

#Linalool graph for subject 9
ggplot(small.scale, aes(x = Concentration, y = IntensityRating.inv)) +
  geom_point() +
  geom_smooth()

#example function#####
#modelTAAR5 <- drm(formula = norm ~ TMAConcentration, data = df.mm1[complete.cases(df.mm1),], fct = LL.4(fixed=c(1,NA,NA,NA),names=(c("Slope", "Top", "Bottom", "ED"))))
# orig1 = function(x){summary(modelTAAR5.orig)$coefficients[2,1] + (summary(modelTAAR5.orig)$coefficients[1,1]-summary(modelTAAR5.orig)$coefficients[2,1])/(1+10^((log10(summary(modelTAAR5.orig)$coefficients[3,1]) - x)*1))}
# orig2 = function(x){summary(modelRho.orig)$coefficients[2,1] + (summary(modelRho.orig)$coefficients[1,1]-summary(modelRho.orig)$coefficients[2,1])/(1+10^((log10(summary(modelRho.orig)$coefficients[3,1]) - x)*1))}
# stat_function(fun = eq1, colour = "#00BFC4") +  

#run HIll model#####
#subset - you can change the inside to anything!
small.scale <- subset(df.scale, Subject == 35 & Type == "Linalool")
small.scale$concentration.log <- log10(small.scale$Concentration)

model.linalool <- drm(formula = IntensityRating.inv~Concentration, data = small.scale, fct = LL.4(fixed = c(1, NA, NA, NA), names = (c("Slope", "Top", "Bottom", "ED"))))
summary(model.linalool)    
linalool.graph = function(x){
  summary(model.linalool)$coefficients[2,1] + (summary(model.linalool)$coefficients[1,1]-summary(model.linalool)$coefficients[2,1])/(1+10^((log10(summary(model.linalool)$coefficients[3,1]) - x)*1))
}

ggplot(small.scale, aes(x = concentration.log, y = IntensityRating.inv)) +
  geom_point() +
  stat_function(fun = linalool.graph)


                