library("tidyverse")
library("lubridate")
library("showtext")
font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
# WD

setwd("C:/Users/hhsie/Dropbox")
# Convert radiants to degrees
deg2rad <- function(deg) {(deg * pi) / (180)}
# Set seed
set.seed(1909)
# Load data from tracker and convert it to minutes and dates
df<-read_csv("track.csv")%>%
mutate(distance=as.numeric(as.Date(`Start date`,"%d/%m/%Y")-
                   as.Date("01/4/2020","%d/%m/%Y")
                   ))%>%
filter(Project%in%c("admin","research","teaching","test"))%>%
mutate(dur=str_remove(Duration, " min"),
min = as.numeric(str_split(dur, ":", simplify = TRUE)[, 1])+
as.numeric(str_split(dur, ":", simplify = TRUE)[, 2])/60)%>%
mutate(day=as.numeric(format(as.Date(`Start date`,"%d/%m/%Y"), format = "%d")),
month=as.numeric(format(as.Date(`Start date`,"%d/%m/%Y"), format = "%m")),
year=as.numeric(format(as.Date(`Start date`,"%d/%m/%Y"), format = "%Y")))

# Create data for circle by converting distance to x and y circular position using sin and cos
labdf<-data.frame(date=c("1/10/2020","15/12/2020","1/6/2020"),
                  label=c("October 2020","December 2020","June 2021"))%>%
  mutate(distance=as.numeric(as.Date(date,"%d/%m/%Y")-
                             as.Date("01/4/2020","%d/%m/%Y")),
                  y=sin(deg2rad(360*distance/365)),
                  x=cos(deg2rad(360*distance/365)))%>%
  mutate(y=ifelse(label=="December 2020",y-.25,y))

# Add y and x coordinates to main data
df<-df%>%
    mutate(y=sin(deg2rad(360*distance/365)),
           x=cos(deg2rad(360*distance/365)))

# Aggregate by project
dfa<-df%>%group_by(Project)%>%summarise(duration=sum(min))%>%
  ungroup()%>%mutate(share=duration/sum(duration))%>%arrange(share)%>%
  mutate(x=-0.7+row_number()/10,label=paste(round(100*share),"",sep=""))

# Aggregate by day of week
dfb<-df%>%mutate(week=week(as.Date(`Start date`,"%d/%m/%Y")))%>%group_by(week)%>%
                 mutate(whours=sum(min))%>%group_by(`Start date`)%>%
      summarise(dur=sum(min,na.rm=TRUE))%>%mutate(wd=weekdays(as.Date(`Start date`,"%d/%m/%Y")))%>%
   group_by(wd)%>%summarise(dur=mean(dur,na.rm=TRUE))%>%
   mutate(dur=(dur/60)/20,weekday=case_when(
     wd=="Monday"~0.1,
     wd=="Tuesday"~0.2,
     wd=="Wednesday"~0.3,
     wd=="Thursday"~0.4,
     wd=="Friday"~0.5,
     wd=="Saturday"~0.6,
     wd=="Sunday"~0.7,
   ),weekday=weekday-0.2,label=round(dur*20,digits=1),weekd=substr(wd,1,3))
#%>%filter(!wd%in%c("Saturday","Sunday"))
         
# Aggegate by week
dfc<-df%>%mutate(week=week(as.Date(`Start date`,"%d/%m/%Y")))%>%group_by(week)%>%
    mutate(n=row_number(),dur=sum(min,na.rm=TRUE))%>%mutate(dur=dur/60)%>%filter(dur>2)%>%ungroup()%>%
    mutate(mean=mean(dur),deviation=dur-mean)%>%filter(n==1)

a<-round(mean(dfc$dur),digits=2)

# Create plot
showtext_auto()
ggplot()+
  geom_text(labdf,mapping=aes(x=x,y=y,label=label),family="bell")+
  geom_jitter(df,mapping=aes(x=x,y=y,colour=Project,size=min),width=0.1,height=0.1)+
  geom_bar(dfa,mapping=aes(x=x,y=share,fill=Project),stat="identity")+
  geom_text(dfa,mapping=aes(x=x,y=share+0.05,colour=Project,label=label),family="bell")+
  geom_text(dfa,mapping=aes(x=x,y=0,colour=Project,label=Project),angle=90,y=-.15,family="bell")+
  geom_bar(dfb,mapping=aes(x=weekday,y=dur),stat="identity")+
  geom_text(dfb,mapping=aes(x=weekday,y=0,label=weekd),angle=90,y=-.085,family="bell")+
  geom_text(dfb,mapping=aes(x=weekday,y=dur+0.05,label=label),family="bell")+
  theme_void()+
  annotate("text", x = 0.1 ,y = 0.6, label = "Average hours by day**",family="bell")+
  annotate("text", x = -0.5 ,y = 0.6, label = "Category shares (in%)",family="bell")+
  annotate("text", x = 0.92 ,y = -1.05, size=4,colour = "#999999",label = "I spent 68 minutes on",family="bell")+
  annotate("text", x =  0.92 ,y = -1.1, size=4,colour = "#999999",label = " an admin task on Feb 4",family="bell")+
  annotate("text", x =  -.42 ,y = 1.2, size=9,colour = "#5b5b5b",label = paste("I worked",a ,"hours per week.*"),family="bell")+
    annotate("text", x =  -.38 ,y = -1.5, size=3,colour = "#5b5b5b",label = paste("By https://github.com/hhsievertsen. * conditional on working at least 2hours. ** conditional on working. "),family="bell")+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#fff8e0"))+ 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_curve(aes(x = 0.85 ,y = -1., xend = 0.7, yend =-0.91),
             colour = "#999999", 
             size=0.75, 
             curvature = +0.2,
             arrow = arrow(length = unit(0.02, "npc")))+ theme(legend.position="none")+
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))




