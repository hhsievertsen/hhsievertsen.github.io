# Births by date
# by Hans H. Sievertsen, hhsievertsen@gmail.com
rm(list=ls())
# load packages
library("tidyverse")
library("extrafont")
library("lubridate")
library("eurostat")
library("gghighlight")
# Load unemployment data from Eurostat
df_raw<-get_eurostat(id="une_rt_m",filters=list(unit="PC_ACT",sex="T", s_adj="NSA",age="TOTAL"))
# Process data
df_clean<-df_raw%>%mutate(date=ymd(time))%>%
  filter(date>dmy("31-12-2005"),
        !is.na(values),
        geo%in%c("US","UK","EU27_2020","DE"))%>%
  mutate(geo=factor(geo,levels=c("EU27_2020","UK","US","DE")))
# Create chart
ggplot(df_clean,aes(x=date,y=values))+
  geom_line(size = 1)+
  theme_minimal()+
  labs(x=" ", y="Unemployment Rate (in %).", title="Monthly Unemployment",
       caption="Source: Eurostat. Percent of active population. Not Seasonally Adjusted. The last observation for the UK is May, 2020. For the other regions it is June, 2020.")+
  facet_wrap(~ geo)+
  theme(strip.text.x = element_text(size = 10, colour = "black",face="bold"),
        plot.title = element_text(hjust = 0.5,size=20,face="bold"),
        plot.caption = element_text(hjust = 0,size=9),
        text=element_text(family="Segoe UI Light"))

 ggsave("fig.png")