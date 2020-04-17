# Income shares in different countries
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
  areas = c("AT"), # Countries
  verbose = TRUE,
  perc = c( "p99p100") #Top 1% 
)



ggplot(wid_data,aes(x=year,y=value,colour=country))+
  geom_line()+
  gghighlight(country=="AT")