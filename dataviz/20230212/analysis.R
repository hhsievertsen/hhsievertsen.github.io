library("tidyverse")
library("haven")
library("ggpubr")
library("gridExtra")
library("EdSurvey")
library("hrbrthemes")
extrafont::loadfonts()
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()
rm(list=ls())


setwd("~/Dropbox/Bucket")

# See guide here: https://www.air.org/sites/default/files/edsurvey-TIMSS-pdf.pdf 
# Download data
# downloadTIMSS(years = 2019, root = "C:\\Users\\B059633\\Dropbox\\Bucket", cache=FALSE)
# Load data

#grade4 <- readTIMSS(path = "C:\\Users\\B059633\\Desktop\\TIMSS\\2019", countries = c("*"), gradeLvl = "4")
grade8 <- readTIMSS(path = "~/Dropbox/Bucket/TIMSS/2019", countries = c("*"), gradeLvl = "8")

# Search
searchSDF(string="bsbm19a", data=grade8)


# Extract data

gddat8 <- getData(data = grade8, varnames = c("idschool","itsex", "bsbm16a","bsmmat01","bsmmat02","bsmmat03","bsmmat04"
                                              ,"bsmmat05","bsbgscm","bsbm19a","bsbm19h","idcntry"),
                  omittedLevels = TRUE, addAttributes = FALSE)

# By country
# Modify data
df8<-bind_rows(gddat8)%>%
  group_by(idcntry)%>%
  mutate(goodatmath=ifelse(bsbm19a%in%c("AGREE A LOT","AGREE A LITTLE"),1,0),
         female=ifelse(itsex=="FEMALE",1,0),
         scoreraw=(bsmmat01+bsmmat02+bsmmat03+bsmmat04+bsmmat05)/5,
         score=(scoreraw-mean(scoreraw))/sd(scoreraw),
         score=100*rank(scoreraw)/length(scoreraw),
         femaleXscore=female*score,
         itsex=ifelse(itsex=="FEMALE","Girls","Boys"))


#  Averages by percentiles
df8c<-df8%>%mutate(score=round(score,1))%>%group_by(score,itsex)%>%
  summarise(y=mean(goodatmath))

# Charts
ggplot()+
  theme_ipsum()+
  geom_point(data=df8c,mapping=aes(x=score,y=y,color=itsex),shape=1)+
  geom_smooth(data=df8,mapping=aes(x=score,y=goodatmath,color=itsex,fill=itsex),
              alpha=.75,size=1.75)+
  labs(x="Test score percentile in Maths",y="Share answering 'I usually do well in mathematics'",
       title="Girls are less likely to say they are good at maths",
       subtitle=" ",
       caption="Data Source: TIMMS 2019. Based on 250 926 8th graders across 46 countries. Notes: The x-variable is the national percentile rank of the average of five plausible value in maths. \n The y-variable is the share answering 'Agree a lot' or 'Agree a little'. The chart is unweighted. Created by hhsievertsen",
       color="")+ 
  scale_y_percent() +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = c(0.9,1.1), )  + 
  guides(color=guide_legend(override.aes=list(fill=NA)),fill="none")


# Globally
# Modify data
df8<-bind_rows(gddat8)%>%
  ungroup()%>%
  mutate(goodatmath=ifelse(bsbm19a%in%c("AGREE A LOT","AGREE A LITTLE"),1,0),
         female=ifelse(itsex=="FEMALE",1,0),
         scoreraw=(bsmmat01+bsmmat02+bsmmat03+bsmmat04+bsmmat05)/5,
         score=(scoreraw-mean(scoreraw))/sd(scoreraw),
         score=100*rank(scoreraw)/length(scoreraw),
         femaleXscore=female*score,
         itsex=ifelse(itsex=="FEMALE","Girls","Boys"))


#  Averages by percentiles
df8c<-df8%>%mutate(score=round(score,1))%>%group_by(score,itsex)%>%
  summarise(y=mean(goodatmath))

# Charts
ggplot()+
  theme_ipsum()+
  geom_point(data=df8c,mapping=aes(x=score,y=y,color=itsex),shape=1)+
  geom_smooth(data=df8,mapping=aes(x=score,y=goodatmath,color=itsex,fill=itsex),
              alpha=.75,size=1.75)+
  labs(x="Test score percentile in Maths",y="Share answering 'I usually do well in mathematics'",
       title="Girls are less likely to say they are good at maths",
       subtitle=" ",
       caption="Data Source: TIMMS 2019. Based on 250 926 8th graders across 46 countries. Notes: The x-variable is the global percentile rank of the average of five plausible value in maths. \n The y-variable is the share answering 'Agree a lot' or 'Agree a little'. The chart is unweighted. Created by hhsievertsen",
       color="")+ 
  scale_y_percent() +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = c(0.9,1.1), )  + 
  guides(color=guide_legend(override.aes=list(fill=NA)),fill="none")




