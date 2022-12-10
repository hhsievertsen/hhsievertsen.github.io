# libraries
library("tidyverse")
library("eurostat")
library("ggthemes")
library("viridis")

# load data
df<-read_csv("tfrnuts3.csv")%>%
    filter(indic_de=="TOTFERRT", TIME_PERIOD==2018)
# map data
dfmap<-get_eurostat_geospatial(output_class = "df", resolution = 60)%>%
       filter(LEVL_CODE == 3)

# merge
mapdata<-merge(dfmap, df,by="geo",all.x=T)%>%
        filter(lat>30,long>-10,long<30,CNTR_CODE!="TR")
ggplot(mapdata, aes(x=long,y=lat,group=group,fill=OBS_VALUE,color=OBS_VALUE)) +
  geom_polygon()+
  theme_tufte()+
  scale_fill_viridis()+
  scale_color_viridis()+
  labs(fill="Total Fertility Rate", color="Total Fertility Rate",x="",y="",
       caption="Data source: Eurostat", title="Total Fertility Rate in 2018")+
  theme(legend.position = c(0.0,0.8),axis.text = element_blank(),
        axis.ticks = element_blank(),legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6),
        legend.key = element_rect(size=1),
        legend.justification='left',
        legend.direction='horizontal')+ 
  coord_map()+
  guides(fill = guide_colourbar(ticks = FALSE,
                                title.position = "top", label.hjust = .5,
                                title.hjust = 0.5,nbin=40, raster=F, barwidth=4),
         color = guide_colourbar(ticks = FALSE,
                                title.position = "top", label.hjust = .5,
                                title.hjust = 0.5,nbin=40, raster=F, barwidth=4))

ggsave("fig2.png")