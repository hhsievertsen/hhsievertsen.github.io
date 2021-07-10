# Birth rate over the last century in Denmark
# by Hans H. Sievertsen, hhsievertsen@gmail.com
# Inspiration: https://kieranhealy.org/blog/archives/2020/02/26/a-new-baby-boom-poster/

rm(list=ls())
# load packages
library(tidyverse)
library(zoo)
library(viridis)
# get birth data using Statistics Denmark API
# Use the console here https://api.statbank.dk/console#data to create query
births<-read_delim("https://api.statbank.dk/v1/data/BEV3A/BULK?delimiter=Semicolon&Tid=*&BEV%C3%86GELSEV=B02",delim=";")
population<-read_delim("https://api.statbank.dk/v1/data/HISB3/CSV?valuePresentation=Value&delimiter=Semicolon&Bev%C3%A6gelse(Stub)=M%2BK&Tid(Stub)=*",delim=";")

# prepare births data
births<-births%>%
        mutate(date=as.yearmon(TID,"%YM%m"),
               year=factor(substr(TID,1,4)),
               month=factor(as.numeric(substr(TID,6,7)),
               levels = 1:12,
               labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
               ordered = TRUE))%>%
        rename(births=INDHOLD)
# prepare population data
population<-population%>%
        mutate(year=factor(TID),pop=(INDHOLD)/12)%>%
        select(year,pop)
# merge
df<-merge(population,births,by="year")%>%
    mutate(birthrate=births/pop)

# create chart
ggplot(df,aes(x=year,y=month))+
  geom_tile(aes(fill = birthrate),colour="white") +
  scale_x_discrete(breaks = seq(1901, 2019, 10)) +
  scale_fill_viridis(option = "viridis")+
  labs(y = NULL) +theme_minimal()+
  theme(legend.position = "top",
        legend.justification = "left",
        legend.title = element_text(size = 18),
        plot.caption = element_text(size = 10))+
  labs(x = " ",     fill = "Births per 1000 Population in Denmark, 1901-2019",
       caption = "Data source: Statistics Denmark")
ggsave("fig.png")
  