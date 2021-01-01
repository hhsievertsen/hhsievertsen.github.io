setwd("C:/Users/hhsie/OneDrive/Desktop")
library("tabulizer")
library("janitor")
library("maps")
library("tidyverse")
library("showtext")
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")

file<-"votes.pdf"


# Extract the table
out<- extract_tables(file)
# Append to one
df<-as.tibble(do.call("rbind", out) )
# Remove first frame and assign names
df<-df%>%
    row_to_names(row_number = 1)%>%
    rename(region=Country,p5=contains("First"),p3=contains("Second"),p1=contains("Third"))%>%
    mutate(Klopp=case_when(p5=="Jürgen Klopp"~5,p3=="Jürgen Klopp"~3,p1=="Jürgen Klopp"~1,TRUE ~0),
           Flick=case_when(p5=="Hans-Dieter Flick"~5,p3=="Hans-Dieter Flick"~3,p1=="Hans-Dieter Flick"~1,TRUE ~0))%>%
    select(-p1,-p3,-p5,-Name)
# UK
uk<-df%>%
    filter(region%in%c("England","Wales", "Northern Ireland", "Scotland"))%>%
    group_by(Vote)%>%
    summarise(Klopp=sum(Klopp),Flick=sum(Flick))%>%
    mutate(region="UK")
df<-rbind(df,uk)
# Cleaning
df<-df%>%
    mutate(region=case_when(region=="Chinese Taipei"~"Taiwan",
                            region=="China PR"~"China",
                            region=="Congo"~"Republic of Congo",
                            region=="Congo DR"~"Democratic Republic of the Congo",
                            region=="IR Iran"~"Iran",
                            region=="Antigua and Barbuda"~"Antigua",
                            region=="Côte d'Ivoire"~"Ivory Coast",
                            region=="Korea Republic"~"South Korea",
                            region=="Korea DPR"~"North korea",
                            region=="Trinidad and Tobago"~"Trinidad",
                            region=="Republic of Ireland"~"Ireland",
                            region=="Mozambique"~"Mayotte",
                            
                            region=="Eswatini"~"Swaziland",
TRUE~region))
# Final cleaning
md<-df%>%
   mutate(dif=Klopp-Flick)%>%
   group_by(region)%>%
   summarise(difference=sum(dif))
# Extract Map
world_map <- map_data("world")
# Merge 
mapdata <- merge(md, world_map, by = "region",all=TRUE)
# Cleaning
mapdata<-mapdata%>%
        filter(!(region %in% c("Antarctica")))%>%
        arrange(region,order)
# Create map
showtext_auto()
ggplot(mapdata, aes(x = long, y = lat,group = group)) +
  geom_polygon(aes(fill=difference))+
  theme_void()+  coord_quickmap()+
  scale_fill_gradientn(breaks=seq(-16,13,4),labels=seq(-16,13,4),
                       colours=c("#bd2626", "grey", "#1a633a"))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "bell"),
        legend.title = element_text(family = "bell",size=12),
        plot.caption= element_text(family = "bell"),
        plot.title = element_text(hjust=0.5,size=20, family = "bell"))+
  guides(fill = guide_colourbar(ticks = FALSE,
         title.position = "top", label.hjust = .5,
         title.hjust = 0.5,nbin=8, raster=F, barwidth=20,
         label.position = "bottom"))+
  labs(fill="Points for Klopp minus points for Flick",
       title="FIFA Best Coach of 2020 Votes",
       caption="The points are the total across media, coach, and captain.")


