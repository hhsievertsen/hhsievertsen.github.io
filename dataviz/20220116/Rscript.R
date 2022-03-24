# load libraries
library("WDI")                  # load World Bank Data
library("tidyverse")            # ggplot2 etc
library("geomtextpath")         # draw legends in line
library("artyfarty")            # theme + colors
library("showtext")             # fint
font_add_google("bell")
# load data
series<-c('SE.PRM.ENRR.MA','SE.PRM.ENRR.FE','SE.SEC.ENRR.MA','SE.SEC.ENRR.FE','SE.TER.ENRR.MA','SE.TER.ENRR.FE')
df<-WDI(indicator=series, country=c('WLD'), start=1960, end=2020)
# data cleaning
df<-df%>%
         pivot_longer(col=4:9,names_to="Series",values_to="Value")%>%   # make long
         filter(Value!="NA")%>%                                         # remove missings     
          mutate(Value=as.numeric(Value),                               # modify columns
            Gender=case_when(grepl("MA",Series)~"Male",grepl("FE",Series)~"Female"),
            Series=case_when(grepl("PRM",Series)~"Primary",grepl("SEC",Series)~"Secondary",grepl("TER",Series)~"Tertiary"))
# create plot
showtext_auto()
    ggplot(df,aes(x=year,y=Value,color=Series,linetype=Gender,label=Gender))+
   geom_textline(fontface=2,hjust=0.1,linewidth=1.5,size=5,key_glyph = "rect",family="bell")+
      theme_farty()+
   theme(legend.position="top",legend.text=element_text(size=17,family="bell"),
   plot.title=element_text(size=30,family="bell"),
   axis.text=element_text(size=18,family="bell",color="black"),
   axis.title=element_text(size=22,family="bell",color="black"))+
   scale_linetype(guide = "none")+
   labs(x=" ",y="Gross enrollment rate (in %)",color="", caption="Data source: World Bank",title="Global school enrollment")+
    scale_color_manual(values = pal("five38"))+
    scale_y_continuous(limit=c(0,110),breaks=seq(0,100,20))
    ggsave("fig.png")
