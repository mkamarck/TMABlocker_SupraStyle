#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(pipeR)

#HINT: for 1 and 2, you may need to specify a directory path
#1. import data
read.csv()

#2. import file that connects valve numbers to stimulus content 

#3. merge the files
merge()

#4. graph the data with Concentration versus IntensityRating.inv
ggplot() +
  #type of plot +
  facet_wrap()#by Type 


#example of plot code (You won't need the subset bit of code for this)
ggplot(data =subset(meltOverallIntensity, variable %in% c("avgTMAalone", "avgGreenAlone", "avgOverallIntensity")), aes(x = factor(variable), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_grid(TMA.Conc~Subject) 