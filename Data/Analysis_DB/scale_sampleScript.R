
library(reshape2)
library(ggplot2)
library(plyr)


#sample scale script

#scale function
scale01 <- function(value, minValue, maxValue) {
  (value - minValue)/(maxValue - minValue)
}

#returns dataframe with the minimum and maximum values for each subject
subjMinMax <- ddply(df.trial_merge, .variables = c("Subject"), .fun = summarize, minValue = min(IntensityRating.inv), maxValue = max(IntensityRating.inv))
#merges the max and min values with the full dataframe
df.scale <- merge(df.trial_merge, subjMinMax)
#calculates the normalized values using the scale function as written
df.scale$normValue <- scale01(df.scale$IntensityRating.inv, df.scale$minValue, df.scale$maxValue)



#another way to scale
df.trial_merge$norm.data <- ddply(df.trial_merge, .variables = c("Subject"), .fun = summarize, scale(IntensityRating.inv))
