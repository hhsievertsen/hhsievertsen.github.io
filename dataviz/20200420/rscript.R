# create chart on igm responses, goals: explore data, flipped bar chart, axis labels, 
# load tidyverse
library("tidyverse")
# load us data
df_us<-read_csv("cleaned_data_US.csv")%>% 
       filter(Qtext=="Question B: With the economy in lockdown, existing gaps in access to quality education between high- and low-income households will be exacerbated.")%>%
  select(Qtext,Vote)
# load european data
df_eu<-read_csv("cleaned_data_EU.csv")%>% 
  filter(Qtext=="Question B: With schools across Europe closed in the lockdown, existing gaps in access to quality education between high- and low-income households will be exacerbated.")%>%
  select(Qtext,Vote)

# calculate percentages
df_combined<-rbind(df_us,df_eu)%>%
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
df_combined<-rbind(df_us,df_eu)%>%
  mutate(mvote=factor(Vote, levels =c("Strongly Disagree","Disagree","Uncertain","Agree","Strongly Agree", "Did Not Answer")))

ggplot(df_combined,aes(x=mvote))+  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_x_discrete(drop = FALSE)+
  theme_light()+
  ylim(0,0.46)+
  coord_flip()+
  labs(x="",y="Share in percent ", 
       title="With the economy in lockdown, existing gaps in access to quality education between \n high- and low-income households will be exacerbated. Vote shares (in percent). ",
       caption="Source: www.igmchicago.org. The chart combines answers from both the European and US panel. ")
