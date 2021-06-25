# libs
library("tidyverse")
library("showtext")
font_add_google("Open Sans", "os")

# load data
df<-read_csv("wbdata.csv")
# make long
df<-df%>%
     pivot_longer(cols=5:65,names_to = "year",values_to = "value")%>%
     mutate(year=as.numeric(substr(year,1,4)),value=as.numeric(value))%>%
     filter(!is.na(`value`))

# the plot

showtext_auto()
ggplot(df,aes(x=year,y=value))+
      facet_wrap(~`Country Name`,ncol=3)+
      geom_line(size=1)+
      theme_minimal()+
      labs(x="",caption="Data source: The World Bank",y="% of primary school age children enrolled",
           title="Primary school enrollment")+
      theme(plot.title = element_text(hjust = 0.5,family="os"),
            panel.spacing = unit(1, "lines"),
            axis.text = element_text(family = "os"),
            axis.title = element_text(family = "os"),
            strip.text = element_text(family = "os"))
ggsave("fig.png")
            