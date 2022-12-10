# libraries
library("tidyverse")
library("ggthemes")
library("viridis")
library("stringr")
# working directory

# load data
df<-read_csv("demo_pjan__custom_2764498_linear.csv")
# Modify variables
df<-df%>%
    mutate(age=ifelse(age=="Y_LT1","Y0",age),age=as.numeric(str_replace_all(age, 'Y', '')))%>%
    filter(!is.na(age),age<75,TIME_PERIOD!=1966)%>%
    group_by(age,TIME_PERIOD,sex)%>%
    summarise(count=sum(OBS_VALUE))%>%
    group_by(TIME_PERIOD,sex)%>%
    mutate(share=count/sum(count),count=ifelse(sex=="F",count,-count),
                 sex=ifelse(sex=="F","Female"," Male"))

# Chart
ggplot(df,aes(y=count,x=age,fill=sex))+
      geom_bar(stat="identity")+
     coord_flip()+
      facet_wrap(~TIME_PERIOD,nrow=1)+
      scale_y_continuous(breaks=c(-800000,-400000,0,400000,800000),
                         labels=c(" ","40","0","40"," "), expand = c(.1, 0))+
  scale_x_continuous(breaks=seq(0,75,15), expand = c(.01, 0))+
      labs(fill="", x="Age (years)",y="Population (thousands)",
           title="The Age Composition of the population in Turkey",
           caption="Data source: Eurostat")+
    theme_tufte()+
  theme(aspect.ratio=6/3,legend.position = "top", axis.text = element_text(size = 14,color="black"),
        legend.text = element_text(size = 14,color="black"),
        legend.key = element_rect(size=0,color="white"),axis.title = element_text(size = 14,color="black"),
        legend.justification='center',legend.box.margin = margin(0, 0, 0, 0),
        legend.direction='horizontal',strip.text = element_text(size = 18, margin = margin()))+
  scale_fill_manual(values=c("#214d42", "#5f9660"))

ggsave("fig.png")