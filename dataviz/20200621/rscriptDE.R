# covid 19 football, by hhsievertsen@gmail.com, 14-6-2020

library("tidyverse")
library("extrafont")
library("ggpubr")
library("xkcd")
library("ggridges")
library("broom")
library("lfe")

setwd("/home/hhs/Documents/covid19football")

rm(list = ls())    
# Load new fonts
xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
download.file(xkcdFontURL,dest="xkcd.ttf",mode="wb")
font_import(".")
loadfonts()
### XKCD theme
theme_xkcd <- theme(
  panel.background = element_rect(fill="white"), 
  axis.ticks = element_line(colour=NA),
  panel.grid = element_line(colour="white"),
  axis.text.y = element_text(colour="black"), 
  axis.text.x = element_text(colour="black"),
  text = element_text(size=10, family="xkcd"),
  plot.title = element_text(hjust = 0.5),
  legend.text=element_text(size=11)
)
# Load data
df<-read_csv("footballdata.csv")%>%
  mutate(outcome=ifelse(apoints==0,"  Heimsieg",ifelse(apoints==1," Unentschieden","Auswärtssieg")))%>%
  filter(!is.na(FTHG))%>%
  mutate(covid19=ifelse(HAdyad=="Werder BremenEin Frankfurt"&season=="1920",1,covid19))  # THIS GAME WAS POSTPONED!


# Create bar chart
dfbar<-df%>%group_by(covid19,outcome)%>%count(outcome)%>%group_by(covid19)%>%
  mutate(frac=100*n/sum(n),out=as.factor(outcome),lab=paste(round(frac,digits=1),"%",sep=""))
p1<-ggplot(dfbar,aes(x=outcome,y=frac,fill=as.factor(covid19)))+ 
  geom_bar(position = 'dodge2', stat = 'identity')+
  geom_text(aes(outcome, frac+1.85, label =lab),family="xkcd",
            position = position_dodge(width = 1),size=3)+theme_xkcd+
  labs(x="",title="A. Gesamt: Heimsieg - Unentschieden - Auswärtssieg",fill="",y="Anteile  in Prozent")+
  scale_fill_discrete(name = " ", labels = c("3284 Spiele Vor COVID19", "55 COVID19 'Geisterspiele'"))

# Scatterplot
dfp<-df%>%group_by(matchday,covid19,season)%>%summarise(apoints=mean(apoints),awin=mean(awin))%>%
  group_by(covid19)%>%mutate(glapoints=mean(apoints))
p2<-ggplot()+ geom_smooth(dfp,mapping=aes(x=matchday,y=apoints,colour=as.factor(covid19),
                                          fill=as.factor(covid19)),alpha=0.25,
                          method=lm,formula=y~1)+
  geom_jitter(dfp%>%filter(covid19==0),mapping=aes(x=matchday,y=apoints,colour=as.factor(covid19)),alpha=0.3)+
  geom_point(dfp%>%filter(covid19==1),mapping=aes(x=matchday,y=apoints,colour=as.factor(covid19)),size=2)+
  theme_xkcd+labs(title="B. Auswärtspunkte pro Spieltag",
                  y="Auswärtspunkte", x="Spieltag")

# Ridges
a<-df%>%filter(covid19==1)
b<-df%>%filter(covid19==00)
dff <- data.frame(x1 = 3, x2 =  mean(a$apoints)*1.025, y1 = 8.95, y2 = 12.0,z=1)
dff2 <- data.frame(x1 = 2.5, x2 = 2.25, y1 = 3.5, y2 = 2.35,z=1)
dff3 <- data.frame(x1 = -0.15, x2 =  mean(b$apoints)*0.997, y1 = 6.25, y2 = 3.35,z=1)
dfp<-dfp%>%mutate(nseas=paste(substr(season,1,2),"/",substr(season,3,4),sep=""))
p3<-ggplot()+
  stat_density_ridges(dfp%>%filter(covid19==0),mapping=aes(x=apoints,nseas,group=season,fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.95),alpha=0.25,
                      colour="white") +
  geom_vline(xintercept = mean(a$apoints),size=2,colour="#00BFC4")+theme_xkcd+
  geom_vline(xintercept = mean(b$apoints),size=2,colour="white")+
  geom_vline(xintercept = mean(b$apoints),size=1.3,colour="#F8766D")+
  labs(y="Saison")+scale_fill_manual(values=c("#F8766D", "red"))+annotate("text", x = 2.4, y = 8.5, label = paste("COVID19 Durchschn:",round(mean(a$apoints),2)),family="xkcd",colour="black",size=4.5)+
  annotate("text", x = 0.5, y = 6.5, label = paste("Durchschn. vor COVID19:",round(mean(b$apoints),2)),family="xkcd",colour="black",size=4.5)+
  annotate("text", x = 2.8, y = 3.75, label = "Obere 5 Prozent",family="xkcd",colour="red",size=3.5)+
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2,group=z),data=dff3,
             colour="black", 
             size=0.5, 
             curvature = +0.2,
             arrow = arrow(length = unit(0.05, "npc")))+ theme(legend.position="none")+
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2,group=z),data=dff,
             colour="black", 
             size=0.5, 
             curvature = +0.2,
             arrow = arrow(length = unit(0.05, "npc")))+ theme(legend.position="none")+
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2,group=z),data=dff2,
             colour = "red", 
             size=0.5, 
             curvature = -0.1,
             arrow = arrow(length = unit(0.03, "npc")))+ theme(legend.position="none")+
  labs(x="Auswärtspunkte",title="C. Punkteverteilung")
###############################



m1<-felm(apoints ~ covid19 | 0  , df)
m2<-felm(apoints ~ covid19 |matchday   , df)
m3<-felm(apoints ~ covid19 |matchday+season   , df)
m4<-felm(apoints ~ covid19 |matchday+season+HAdyad   , df)

mf1<-tidy(m1,conf.int = TRUE, conf.level = 0.95, )%>%filter(term=="covid19")%>%
  mutate(model="Roher Punkteunterschied  ",beta=estimate,ul=conf.high,ll=conf.low)%>%
  select(model,beta,estimate,ul,ll)

mf2<-tidy(m2,conf.int = TRUE, conf.level = 0.95, )%>%filter(term=="covid19")%>%
  mutate(model=" + Spieltag Fixed Effects  ",beta=estimate,ul=conf.high,ll=conf.low)%>%
  select(model,beta,estimate,ul,ll)

mf3<-tidy(m3,conf.int = TRUE, conf.level = 0.95, )%>%filter(term=="covid19")%>%
  mutate(model=" + Saison Fixed Effects    ",beta=estimate,ul=conf.high,ll=conf.low)%>%
  select(model,beta,estimate,ul,ll)

mf4<-tidy(m4,conf.int = TRUE, conf.level = 0.95, )%>%filter(term=="covid19")%>%
  mutate(model="  + H & A Team Dyade FE",beta=estimate,ul=conf.high,ll=conf.low)%>%
  select(model,beta,estimate,ul,ll)
pl<- dplyr::bind_rows(mf1,mf2,mf3,mf4)  

p4<-  ggplot(pl,aes(x=model,y=estimate))+geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=ll, ymax=ul), width=.2,
                position=position_dodge(.9)) +theme_xkcd+labs(title="D. Regressionsergebnisse               ")+
  coord_flip()+labs(x="",y="Estimat: Auswärtspunkte X COVID19 ")+geom_label(mapping=aes(x=model,y=estimate,label=round(estimate,2)),family="xkcd")
fig<-ggarrange(p1,p2,p3,p4,nrow = 2,ncol=2,common.legend = TRUE, 
               legend="top")

title <- expression(atop(bold("Geisterspiele und der Heimvorteil?"), scriptstyle("-Eine Analyse von 3339 Bundesliga-Spielen-")))
annotate_figure(fig,
                top = text_grob(title, color = "black", face = "plain", size = 20,family = "xkcd"),
                bottom = text_grob("Daten von www.football-data.co.uk & football-data.org. Von hhsievertsen - github.com/hhsievertsen/homecrowd. Ver: 14. June,2020.", color = "grey",
                                   hjust = 1, x = 1, face = "italic", size = 8),
                
)
ggsave("homecrowdfigDE.png",width=8,height=6)
