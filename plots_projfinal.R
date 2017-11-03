library(dplyr)
library(ggthemes)
library(ggplot2)
plots<-read.csv(file.choose())


##depending on the sms_received what is the status 
ggplot(plots, aes(x=SMS_received, fill=No.show)) + geom_bar(position="fill")

##it is evident that the sms received has nothing to do with the turn up



ggplot(data = plots, aes(x = Age, colour = Gender))+
  geom_density()+
  geom_vline(xintercept = 16, linetype = 'longdash')+
  geom_vline(xintercept = 68, linetype = 'longdash')+
  theme_igray()+
  ggtitle('Density of Age by Gender')


plots %>%
  group_by(Age, No.show) %>%
  summarise(Count = n()) %>%
  mutate(Freq = Count/sum(Count)*100) %>%
  filter(No.show  == 'Yes' & Count >29) %>%
  ggplot(aes(x = Age, y = Freq))+
  geom_point()+
  stat_smooth(method = 'loess')+
  geom_vline(xintercept = 18, linetype = 'longdash')+
  geom_vline(xintercept = 72, linetype = 'longdash')+
  ggtitle('No-Show % by Age')+
  ylab('No-Show %')+
  theme_igray()

##No show rates are higher when the age is less than 25
##No show rates are lesser for people who are old.





ggplot(plots,aes(x=Gender,y=No.show,fill=Gender)) + geom_bar(stat="identity")+ggtitle("No Show Rate by gender")

