# libraries
library("tidyverse")
library("gghighlight")
# working directory
setwd("~/Documents/GitHub/hhsievertsen.github.io/dataviz/20210815")
# load data
df<-read_csv("onsdata.csv")
# make long and modify data
df<-df%>%
    pivot_longer(cols=2:13,names_to = "Month",values_to = "Deaths")%>%
    mutate(Month=factor(Month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
    Period=case_when(
      Year==2021~"March 2020-June 2021",
      Year==2020&!Month%in%c("January","February")~"March 2020-June 2021",
      TRUE~""))
df$date <- as.Date(paste0(df$Month, df$Year, "01"), format="%B%Y%d")
df<-df%>%group_by(Month)%>%mutate(deviation=Deaths-mean(Deaths,na.rm=TRUE))
# make chart
ggplot(df,aes(x=date,y=deviation))+
  geom_vline(xintercept=as.Date("March202001", format="%B%Y%d"),linetype="dashed",color="grey")+
  geom_line(color="#E69F00",size=1)+
  geom_line(aes(color=Period),size=1)+
  theme_classic()+
  labs(x="",y="Deviation in deaths relative to month average",
       title="Deaths in England relative to month specific average",
       caption="Data source: ONS")+
  theme(legend.position = "none",plot.title = element_text(hjust=0.5))+
  geom_text(aes(x=as.Date("May202101", format="%B%Y%d"),y=45000,label="Pandemic"))+
  scale_color_manual(values = c( "#E69F00", "#56B4E9"))

ggsave("fig.png")