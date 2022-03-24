# Poverty and Life Expectancy
# Set WD
setwd("/Users/hhs/Dropbox/My Mac (Hanss-MacBook-Air.local)/Documents/GitHub/hhsievertsen.github.io/dataviz/20220115")
# Load libraries 
library("tidyverse")
library("readxl")
# Load main data file from the World Bank
df<-read_excel("Data_Extract_From_World_Development_Indicators.xlsx",sheet="Data")
# Remove missing values 
df<-filter(df,!is.na(Value))
# Keep  only most recent observation. 
df<-arrange(df,`Country Name`,`Series Name`,`Time`)
df<-group_by(df,`Country Name`,`Series Name`)
df<-filter(df,row_number()==n())
df<-ungroup(df)
# Backup to calculate year by group
yeardata<-group_by(df,`Country Name`)
# Make the dataset wider
df<-pivot_wider(df,names_from=`Series Name`,id=`Country Name`,values_from=`Value`)
# Rename columns (see section 5.1 in the tutorial)
df<-rename(df,PovertyHC=contains("Headcount"),
              PovertyGAP=contains("gap"),
              LifeExpectancy=starts_with("Life"),
              Country=starts_with("Country"))

# Create a new column which is the logarithm of the poverty rate 
df<-mutate(df,LogPovertyHC=log(PovertyHC),LogPovertyGAP=log(PovertyGAP))
# Now we also want to get the region and income group, so we load the metadata
df_region<-read_excel("Data_Extract_From_World_Development_Indicators.xlsx",sheet="Country - Metadata")
# Keep columns that we need (
df_region<-select(df_region,`Income Group`, `Table Name`, `Region`)
# Rename columns
df_region<-rename(df_region,Country=starts_with("Tab"),IncomeGroup=starts_with("Inc"))
# Merge the new dataset with information on the region to the old dataset 
df<-merge(df,df_region,by="Country")
# Remove missing Group values 
df<-filter(df,!is.na(IncomeGroup))
# Creare chart
ggplot(df, aes(x=PovertyHC, y=LifeExpectancy,
               colour=IncomeGroup,size=PovertyGAP))+
  geom_jitter(width=.1,height=.1)+
  labs(title="Life expectancy and extreme poverty",
       caption="Data source: The World Bank. Each scatter is a country. Data is for the most recent year within the period 2011-2020. ",
       x="Share of the population w. daily income <5.5$",
       size="Poverty Gap at $5.5 (in %)",y="Life Expectancy", color=" ")+
  theme(legend.position="top",
        legend.title=element_text(colour="#6e6e6e",size=8),
        legend.key.height= unit(.25, 'cm'),
        legend.key.width= unit(.25, 'cm'),
       panel.background = element_blank(),
       panel.grid.major=element_line(color="grey",size=0.1),
              panel.grid.minor=element_line(color="grey",size=0.1),
        plot.caption = element_text(hjust=0,colour="#6e6e6e",size=8) )+
  scale_color_manual(values=c("#999999", "#63a875","#E69F00", "#56B4E9"))+
  theme(legend.direction = "horizontal", legend.box = "vertical",
        legend.margin = margin(t=0,b=0,unit='pt'))+
  guides(size = guide_legend(order = 2,
                             override.aes=list(colour="#6e6e6e")),
         label.vjust =15)
         ggsave("fig.png")
