# create chart on igm responses, goals: explore data, flipped bar chart, axis labels, 
# load tidyverse
library("tidyverse")
setwd("C:\\Github\\hhsievertsen.github.io\\dataviz\\20200420")
# load data
df_combined<-read_csv("igm_survey_lockdown_education_inequality.csv")

# calculate percentages
df_combined<-df_combined%>%
  group_by(Vote) %>%
  summarise(count=n()) %>% 
  mutate(perc=100*count/sum(count))%>%
  filter(Vote!="Did Not Answer")%>%
  mutate(mvote=factor(Vote, levels =c("Strongly Disagree","Disagree","Uncertain","Agree","Strongly Agree")))

# Create chart
ggplot(df_combined,aes(x=mvote,y=perc))+geom_bar(stat="identity")+
  scale_x_discrete(drop = FALSE)+                                 # show all factors!
   theme_void()+                                                  # simple layout
  ylim(0,46)+                                                     
  coord_flip() +
  theme(plot.title = element_text(size=18),axis.text.y=element_text(),
        plot.caption=element_text(size=12,hjust = 0.5,vjust=2.5),
        )+
    labs(x="",y=" ", 
       title="With the economy in lockdown, existing gaps in access to quality education between \n high- and low-income households will be exacerbated. Vote shares (in percent). N=90.",
       caption="Source: www.igmchicago.org. The chart combines answers from both the European and US panel. Did Not Answer (5.6%).\n Note that the wording of the question differed slightly between the European and US panel.")+
  geom_text(aes(label=paste(" ", round(perc),"%",sep="")),hjust=-0.25,size=5)+
  geom_text(aes(y=1,x=1,label="0%"))
ggsave("fig.png")







# Create chart without computing percentages first
df_combined<-read_csv("igm_survey_lockdown_education_inequality.csv")
df_combined<-df_combined%>%
  mutate(mvote=factor(Vote, levels =c("Strongly Disagree","Disagree","Uncertain","Agree","Strongly Agree", "Did Not Answer")))

ggplot(df_combined,aes(x=mvote))+  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_x_discrete(drop = FALSE)+
  theme_light()+
  ylim(0,0.46)+
  coord_flip()+
  labs(x="",y="Share ", 
       title="With the economy in lockdown, existing gaps in access to quality education between \n high- and low-income households will be exacerbated. Vote shares (in percent). ",
       caption="Source: www.igmchicago.org. The chart combines answers from both the European and US panel. ")
