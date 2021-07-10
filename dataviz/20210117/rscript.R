# quick and dirty 
library("tidyverse")
library("showtext")
font_add_google("Schoolbell", "bell")
showtext_auto()


# Load data
df<-read_csv("imdbdata.csv")%>%                          # Load dataset from https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset
  select(imdb_title_id,mean_vote,starts_with("votes_"))%>% # Select variables
  filter(imdb_title_id%in%c("tt0468569","tt0068646","tt0120737","tt0111161"))%>% # Select movies
  mutate(movie=case_when(imdb_title_id=="tt0468569"~"The Dark Knight",
                         imdb_title_id=="tt0068646"~"The Godfather",
                         imdb_title_id=="tt0120737"~"The Lord of the Rings (2001)",
                         imdb_title_id=="tt0111161"~"The Shawshank Redemption"))%>% # From movie id to movie name
  pivot_longer(starts_with("votes_"), names_to = "rating", values_to = "freq",
               names_prefix = "votes_",names_transform=list(rating = as.integer))%>% # Make long
  group_by(movie)%>%
  mutate(relfreq=100*freq/sum(freq),label=paste(round(relfreq,digits=1),"%",sep=""),titles=paste(movie,"(mean:",mean_vote,")"))


# Create plot

ggplot(df,aes(x=rating,y=relfreq,group=movie))+
  geom_bar(stat="identity")+
  facet_wrap(~titles)+
  theme_minimal()+
  geom_text(aes(label=label),nudge_y = 4,size=4,family="bell")+
  labs(y="Percent of votes",x="Rating", title="Distribution of user votes on IMDb ",
       caption="Data source: https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset")+
  theme(plot.title = element_text(hjust=0.5,size=25, family = "bell"),
        strip.text=element_text(hjust=0.5,size=13, family = "bell"),
        axis.text =element_text(hjust=0.5,size=10, family = "bell") ,
        axis.title = element_text(hjust=0.5,size=10, family = "bell"),
        plot.caption =element_text(hjust=0,size=8, family = "bell") )+
  scale_x_continuous(breaks=seq(1,10,by=1))

```
