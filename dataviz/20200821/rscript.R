# Births by date
# by Hans H. Sievertsen, hhsievertsen@gmail.com
rm(list=ls())
# load packages
library(tidyverse)
library(extrafont)
# get birth data using Statistics Denmark API
# Use the console here https://api.statbank.dk/console#data to create query
births<-read_delim("https://api.statbank.dk/v1/data/FODDAG/CSV?valuePresentation=Value&delimiter=Semicolon&FDAG(Head)=*&FMAANED(Head)=*&Tid(Head)=*",delim=";")
# aggregate across years
agg<-births%>%
  filter(FDAG!="I alt",FMAANED!="I alt",!is.na(INDHOLD))%>%
  group_by(FMAANED,FDAG)%>%
  summarise(births=mean(INDHOLD))
## Modify data
df<-agg%>%
  mutate(deviation=100*((births/mean(births,na.rm=TRUE))-1))%>%
  mutate(month=case_when(
    FMAANED=="Januar" ~ 1,
    FMAANED=="Februar" ~ 2,
    FMAANED=="Marts" ~ 3,
    FMAANED=="April" ~ 4,
    FMAANED=="Maj" ~ 5,
    FMAANED=="Juni" ~ 6,
    FMAANED=="Juli" ~ 7,
    FMAANED=="August" ~ 8,
    FMAANED=="September" ~ 9,
    FMAANED=="Oktober" ~ 10,
    FMAANED=="November" ~ 11,
    FMAANED=="December" ~ 12),
    day=parse_number(FDAG),
    month=factor(month,
                 levels = rev(1:12),
                 labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                 ordered = TRUE))%>%
  select(month,day, deviation)


# create chart
ggplot(df,aes(x=day,y=month))+
  geom_tile(aes(fill = deviation,colour=deviation), show.legend = FALSE)+
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",midpoint = 0,na.value = "white", guide = "colourbar")+
  scale_colour_gradient2(low = "darkred",mid = "white",high = "darkgreen",midpoint = 0,na.value = "white", guide = "colourbar")+
  theme_bw()+
  labs(x="", y="")+
  theme(panel.grid=element_blank(), panel.border=element_blank(),
        axis.ticks = element_blank())+
  geom_text(df%>%filter(deviation>(-20)),mapping=aes(x=day,y=month,
            label=sprintf("%0.0f", round(deviation, digits = 1))),size=3,colour="black",family="Comic Sans MS")+
  geom_text(df%>%filter(deviation<(-20)),mapping=aes(x=day,y=month,
            label=sprintf("%0.0f", round(deviation, digits = 1))),size=3,colour="white",family="Comic Sans MS")+
  scale_x_continuous(limits=c(0,32),breaks=seq(1,31,by=2))+
  coord_equal()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        axis.text.y=element_text( margin=unit(c(0,-1,0,0), "cm")),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.1,family="Comic Sans MS"),
        text=element_text(family="Comic Sans MS"))+
  labs(title="Births in Denmark relative to the daily average (in %)",
 caption="Notes: Based on all births 2007-2019. Data source: Statistics Denmark table 'FODDAG'. For R source code check: https://hhsievertsen.github.io")
 ggsave("fig.png")