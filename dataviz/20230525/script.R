library("tidyverse")
library("gganimate")

df<-data.frame(position=c(1,1,1,1,3,3,5,3,3,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,1,1,1,2,
                          7,2,7,5,2,5,2,4,4,8,5,4,4,6,6,6,5,4,3,3,2,2,2,2,1,2,2,2,1,2,2,2,1),
               team=c(rep("Bayern München",33),rep("Borussia Dortmund",33)),
                gw=rep(1:33,2))
df<-df%>%mutate(label=paste(team,": ",position,".",sep=""))%>%arrange(gw)
labeldata<-data.frame(team="Borussia Dortmund",
  x=48,y=1:8,label=rep("             g                                               ",8)
)


p<-ggplot(df,aes(x=gw,y=position,color=team,group=team))+
  geom_label(data=labeldata,mapping=aes(x=x,y=y,label=label),
             fill="black",color="black")+
  geom_line(size=2)+
  theme_minimal() +
  scale_y_reverse(expand = c(0, 0),limits=c(9,0.5),breaks=seq(9,1,-1))+
  scale_colour_manual(values=c("red","yellow"))+ 
  geom_point(color="grey",size=2)+
  geom_text(aes(x=46.5,label=label),size=6)+
  geom_segment(aes(xend=34.5,yend=position,colour=team),linetype=2)+
  labs(x="Game Week",y="League Position",color="",title="Bayern & Dortmund 2022/23", 
       subtitle='Matchday: {(frame_along)}',caption="by https://github.com/hhsievertsen")+
  theme(panel.background = element_rect(fill="black"),
        plot.background =  element_rect(fill="black"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text = element_text(color="white", family="mono"),
        axis.title =  element_text(color="white", family="mono"),
        plot.caption = element_text(hjust = 1,size=10,family="mono"),
        title=element_text(color="white",size=20,hjust=0.5,family="mono"))+
  scale_x_continuous(breaks=seq(1,34,4),limits=c(1,55))+
  transition_reveal(gw)+
  theme(panel.background = element_rect(fill="black"))

#animate(p,end_pause =10, nframes = 15, fps=2)


anim_save("buli.gif", p,end_pause =400, nframes = 1500, fps=30)
