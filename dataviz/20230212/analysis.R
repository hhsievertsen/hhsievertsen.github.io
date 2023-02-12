library("tidyverse")
library("haven")
library("ggpubr")
library("gridExtra")
library("EdSurvey")
library("hrbrthemes")
rm(list=ls())


setwd("C:\\Users\\B059633\\Desktop")

# See guide here: https://www.air.org/sites/default/files/edsurvey-TIMSS-pdf.pdf 
# Download data
# downloadTIMSS(years = 2019, root = "C:\\Users\\B059633\\Desktop", cache=FALSE)
# Load data

grade4 <- readTIMSS(path = "C:\\Users\\B059633\\Desktop\\TIMSS\\2019", countries = c("*"), gradeLvl = "4")
grade8 <- readTIMSS(path = "C:\\Users\\B059633\\Desktop\\TIMSS\\2019", countries = c("*"), gradeLvl = "8")

# Search
searchSDF(string="TOTWGT", data=grade8)


# Extract data

gddat4 <- getData(data = grade4, varnames = c("itsex", "asbm02a","asmmat01","asbgscm","asbm05g","asbm05h","idcntry"),
                 omittedLevels = TRUE, addAttributes = FALSE)
gddat8 <- getData(data = grade8, varnames = c("itsex", "bsbm16a","bsmmat01","bsbgscm","bsbm19g","bsbm19h","idcntry"),
                  omittedLevels = TRUE, addAttributes = FALSE)


# Modify data
df4<-bind_rows(gddat4)%>%
  mutate(goodatmath=ifelse(asbm05g%in%c("AGREE A LOT","AGREE A LITTLE"),1,0),
         female=ifelse(itsex=="FEMALE",1,0),
         score=(asmmat01-mean(asmmat01))/sd(asmmat01),
         score=100*rank(asmmat01)/length(asmmat01),
         femaleXscore=female*score,
         itsex=ifelse(itsex=="FEMALE","Girls","Boys"))

df8<-bind_rows(gddat8)%>%
  mutate(goodatmath=ifelse(bsbm19g%in%c("AGREE A LOT","AGREE A LITTLE"),1,0),
         female=ifelse(itsex=="FEMALE",1,0),
         score=(bsmmat01-mean(bsmmat01))/sd(bsmmat01),
         score=100*rank(bsmmat01)/length(bsmmat01),
         femaleXscore=female*score,
         itsex=ifelse(itsex=="FEMALE","Girls","Boys"))



# Overall chart
df8c<-df8%>%mutate(score=round(score,1))%>%group_by(score,itsex)%>%
     summarise(y=mean(goodatmath))

ggplot()+
  geom_point(data=df8c,mapping=aes(x=score,y=y,color=itsex),shape=1)+
   geom_smooth(data=df8,mapping=aes(x=score,y=goodatmath,color=itsex,fill=itsex),
               alpha=.2,size=1.75)+
  labs(x="Test score percentile in Maths",y="Share answering 'I am good at Maths'",
       title="Girls are less likely to say they are good at Maths",
       subtitle="for any given level of Maths skills",
       caption="Data Source: TIMMS 2019. Based on 250 926 8th graders across 46 countries. \n Notes: The x-variable is the global percentile rank of the 1st plausible value in mathematics.\n  The y-variable is the share answering 'Agree a lot' or 'Agree a little'. The chart is unweighted. \n by hhsievertsen",
       color="")+ 
  scale_y_percent() +
  theme_ipsum_rc()+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = c(0.15,0.9))  + 
  guides(color=guide_legend(override.aes=list(fill=NA)),fill="none")
