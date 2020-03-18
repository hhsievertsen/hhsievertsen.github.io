# age at birth by hhs
library("gganimate")
library("tidyverse")
library("eurostat")
# data
df<-get_eurostat("demo_fordagec")%>%          # load eurosat data
    filter(geo=="UK",ord_brth=="1")%>%       # UK and first born only
    filter(!age%in%c("TOTAL","UNK"),!substr(age,4,4)%in%c("-","E")) %>% #remove age groups
    mutate(age=as.numeric(substr(age,2,3)),year=as.numeric(substr(as.character(time),1,4)))%>% #year variable
    group_by(year)%>% 
    mutate(share=100*values/sum(values)) #share

# create dynamic graph
my_gif<-ggplot(df%>%filter(year<2013),aes(x=age,y=share,fill=age))+geom_bar(stat="identity")+
  labs(y="Share in percent",title = 'Age of first-time mothers in the UK in year: {frame_time}',x="Age")+
  theme( axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),panel.background = element_blank(), panel.grid.major.y = 
  element_line(color = "grey80"))+scale_y_continuous(breaks =seq(0,10,by=2))+
  scale_x_continuous(breaks = seq(15, 45, by = 5))+
  transition_time(as.integer(year)) +
  guides(fill=FALSE)+
  ease_aes('sine-in-out')
p<-animate(my_gif, end_pause=15)
anim_save("mygif.gif", p)
