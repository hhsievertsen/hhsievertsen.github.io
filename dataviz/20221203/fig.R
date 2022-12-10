# libraries
library("tidyverse")
library("ggthemes")
library("viridis")
library("gghighlight")

# load data
df<-read_csv("demo_mlexpec__custom_2815162_linear.csv")%>%
    filter(geo=="SE")%>%
    arrange(TIME_PERIOD)%>%
    mutate(change=100*((OBS_VALUE/lag(OBS_VALUE))-1))
  

# My function
scalefunc<-function(y){
  (y+36)*2
}
# Chart

ggplot(df)+
  geom_line(aes(y=OBS_VALUE/2-36,x=TIME_PERIOD),size=1.5,color="grey")+
  geom_line(aes(y=OBS_VALUE/2-36,x=TIME_PERIOD,color=lead(change)),size=1.5)+
  geom_bar(aes(y=change,x=TIME_PERIOD,fill=change,color=change),stat="identity")+
  theme_tufte()+
  labs(color="Year to year change", fill="Year to year change" ,
       y="Year to year change                                                                                 \n in life expectancy                                                                                 ", x="",
       title="Life expectancy in Sweden",caption="Data source: The World Bank")+
  scale_y_continuous(limits=c(-1,6),breaks=seq(-1,1,0.5),labels=c("-1.0%","-0.5%","0.0%","0.5%","1.0%"),
                     sec.axis = sec_axis(trans=~ scalefunc(.), breaks=c(74,76,78,80,82,84),
                                         labels=paste(seq(74,84,2),"y",sep="") , name = "Life expectancy at birth                                              "))+
  geom_rangeframe(data=data.frame(x=c(1970,2020), y=c(-1,1)), aes(x, y))+
  scale_x_continuous(limits=c(1970,2022),breaks=seq(1970,2020,10))+
  geom_rangeframe(data=data.frame(x=c(1970,2020), y=c(1,6)), aes(x, y),sides="r")+
  theme(legend.position = c(0.1,0.8),legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6),
        legend.key = element_rect(size=1),
        legend.justification='left',
        legend.direction='horizontal')+
  scale_fill_gradientn(breaks=seq(-0.8,0.8,0.4),labels=c("-0.8%","","0.0%"," ","0.8%"),
                       colours=c("#bd2626", "grey", "#1a633a"))+
  scale_color_gradientn(breaks=seq(-0.8,0.8,0.4),labels=c("-0.8%","","0.0%"," ","0.8%"),
                       colours=c("#bd2626", "grey", "#1a633a"))+
  guides(fill = guide_colourbar(ticks = FALSE,
                                title.position = "top", label.hjust = .5,
                                title.hjust = 0.5,nbin=5, raster=F, barwidth=4),
         color = guide_colourbar(ticks = FALSE,
                                 title.position = "top", label.hjust = .5,
                                 title.hjust = 0.5,nbin=5, raster=F, barwidth=4))
  

    
ggsave("fig.png")