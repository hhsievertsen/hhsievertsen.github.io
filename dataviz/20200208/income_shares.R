# Create a dynamic chart of income shares
# clear workspace
rm(list=ls())

# load data from the World Inequality Database
# devtools::install_github("WIDworld/wid-r-tool")
# Load the package
library("wid")
wid_data <- download_wid(
  indicators = "sptinc", #  Pre-tax national income 
  areas = c("AT", "DE", "GB","WO", "BE", "CZ", "DK", "NL", "SE","PL"), # Countries
  perc = c("p0p50","p90p99", "p99p100") # Bottom 50%, Top 1% and top 10%
)

# Create an animation
# install.packages("tidyverse","gganimate")
library("tidyverse")
library("gganimate")


# Select country, create labels etc
df<-wid_data%>%
  filter(country=="GB")%>%
  mutate(percent=100*value, # share in percent
  labelvar=sprintf("%0.2f", round(percent, digits = 2)))  # percent with 2 decimal points for label.

# Create chart 
p<-ggplot(df,aes(x=year,y=percent,colour=percentile))+
  geom_line(size=1)+  transition_reveal(year)+
  geom_segment(aes(xend = 2016, yend = percent,colour=percentile), linetype = 2)+
  geom_point(size = 2) +
  geom_text(aes(x = 2017, label = labelvar), hjust = 0)+
  labs(title = 'Income shares in the UK', y = 'Share of pre-tax national income (%)',
       caption="Data source: World Inequality Database.",x=" ",color=" ") + 
  theme_minimal() + xlim(1980,2020)+ theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "top")+
  scale_y_continuous(breaks=seq(0,30,by=5),limits = c(0,25))+
  scale_color_discrete(label=c("Bottom 50%", "P90-P99", "Top 1%"))
animate(p, end_pause=15)
anim_save("income_shares.gif")
