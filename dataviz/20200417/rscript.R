# Income shares in different countries, main goal: use gghighlight & coord_cartesian
setwd("C:\\Users\\hs17922\\Dropbox\\Work\\Research\\Blandede bolsjer\\igm on education")
# clear workspace
rm(list=ls())
# Install packages
# devtools::install_github("WIDworld/wid-r-tool")
# install.packages("gghighlight")
# Load the packages
library("wid")
library("tidyverse")
library("gghighlight")
wid_data <- download_wid(
  indicators = "sptinc", #  Pre-tax national income 
  areas = c("all"), # Countries
  verbose = TRUE,
  perc = c( "p99p100") #Top 1% 
)
# select what we want
df<-wid_data%>%
  filter(variable=="sptinc992j",
         str_length(country)==2, # Exclude US states
         !(country%in%c("QB","QC","QD","QE","QF","QG","QH","QI","QJ","QK",
                          "QL","QM","QN","QO","QP","QQ","QR","QS","QT","QU","QV","QW","QX","QY")), # Exclude regions
       ) 
         
         


ggplot(df,aes(x=year,y=value,colour=country))+
  geom_line(size = 1.5)+
  gghighlight(country%in%c("US","IN","FR","GB","RU"))+
  theme_light(base_size = 25)+
  labs(x="Year", y="Top 1% income share", caption="Source: World Inquality Database", 
       title="Top 1% Income share (all countries)")+
  coord_cartesian(ylim = c(0, 0.35)) # truncate y axis

ggsave("fig.png")