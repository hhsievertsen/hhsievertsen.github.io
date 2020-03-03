library("tidyverse")
library("zoo")
library("ggforce")
library("ggseas")
rm(list=ls())
# get birth data using Statistics Denmark API
# Use the console here https://api.statbank.dk/console#data to create query
births<-read_delim("https://api.statbank.dk/v1/data/BEV3A/BULK?delimiter=Semicolon&Tid=*&BEV%C3%86GELSEV=B02",delim=";")
population<-read_delim("https://api.statbank.dk/v1/data/HISB3/CSV?valuePresentation=Value&delimiter=Semicolon&Bev%C3%A6gelse(Stub)=M%2BK&Tid(Stub)=*",delim=";")

# prepare births data
births<-births%>%
  mutate(date=as.yearmon(TID,"%YM%m"),
         year=as.numeric(substr(TID,1,4)))%>%
  rename(births=INDHOLD)
# prepare population data
population<-population%>%
  mutate(year=factor(TID),pop=(INDHOLD)/12)%>%
  select(year,pop)
# merge
df<-merge(population,births,by="year")%>%
  mutate(birthrate=births/pop)
 
# plot
ggplot(df,aes(x=date,y=birthrate))+
  geom_line(colour = "grey80")+
  stat_stl(s.window = 24,x13_params = list(x11 = "", outlier = NULL))+
  facet_zoom(xy=date <as.yearmon("1953M01","%YM%m")&
               date >as.yearmon("1940M01","%YM%m"), zoom.size = .75,
              horizontal = FALSE)+labs(x = " ",     
                                                    title = "The 1940s Baby Boom in Denmark",
                                                    caption = "Data source: Statistics Denmark",y="Births per 1000 population.")+
  theme_classic()+
  theme(legend.title = element_text(size = 25),
        plot.title = element_text(hjust = 0.5))
ggsave("fig.png") 
  