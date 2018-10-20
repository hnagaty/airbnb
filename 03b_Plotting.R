library(ggplot2)
library(dplyr)

trainUsersPlot <- allUsersExpanded %>%
  filter(dataset=="train")

trainUsersPlot %>%
  filter (!(ageRange %in% c("[80,85)","[85,90)","[90,95)","[95,100)","[100,105)"))) %>%
  ggplot(aes(x=ageRange,fill=gender)) +
  geom_bar() +
  labs(title="Age & gender distribution",x="Age",y="Count",fill="Gender")
  
       