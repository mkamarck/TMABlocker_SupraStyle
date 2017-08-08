#from DR Jars
#some tips on making graphs
#plot just Dardalie
df.jars$Session2 <- factor(df.jars$Session, levels = c("1", "2"), labels = c("Not Agitated", "Agitated")) #this is how you change what the legend says
ggplot(data = subset(df.jars, Subject == 1000 & Session2 %in% c("Not Agitated", "Agitated")), aes(x = Concentration, y = IntensityRating.inv, color = Session2)) +
  geom_point() +
  ggtitle("Effect of agitating on TMA odor Intensity")+
  xlab("Intensity Rating") +
  ylab("Concentration of TMA")