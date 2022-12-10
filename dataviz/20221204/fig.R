# libraries
library("tidyverse")
library("ggthemes")
library("viridis")
library("stringr")
library("gghighlight")
# working directory
# load data
df<-read_csv("afe323d4-6ea0-470a-b9a8-3f7fecf75051_Data.csv")%>%
   select(-66)%>%
   pivot_longer(cols=5:65,names_to="Year",values_to="Value")%>%
   mutate(year=as.numeric(substr(Year,1,4)))%>%
  filter(!is.na(`Country Code`))
# Chart

ggplot()+
      geom_line(df,mapping=aes(y=Value,x=year,group=`Country Name`))+
      gghighlight(use_direct_label = F) +
      facet_wrap(~ `Country Name`)+
      theme_tufte()+
  theme(plot.title = element_text(hjust=0.5))+
      scale_x_continuous(breaks=seq(1960,2020,25))+
     labs(y="Life expectancy at birth (years)", x="",
          title="Life Expectancy in 16 randomly\n selected countries",
          caption="Data source: The World Bank")

ggsave("fig.png")