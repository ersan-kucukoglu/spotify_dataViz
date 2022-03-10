
#import the libraries
rm(list = ls())
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(modelsummary)
library(gganimate)

#load the data
spotify <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")
summary(spotify$track_popularity)

boxplot(spotify$track_popularity)
boxplot.stats(spotify$track_popularity)$out
###################
#Data Cleaning&Transformation

# check frequency by playlist_genre
datasummary( playlist_genre ~ N + Percent() , data = spotify)

# save the track_album_release_date as a date
spotify$track_album_release_date <- as.Date(spotify$track_album_release_date)

#extract the year
spotify$year <- as.numeric(format(spotify$track_album_release_date, "%Y"))

#take the mean of the track popularity
mean_popularity<- mean(spotify$track_popularity)

###################
#Data Visualization
# 1) The most popular playlist genres by average popularity

genre_by_popularity <- spotify[,.(avg_popularity=mean(track_popularity)),by=playlist_genre][order(desc(avg_popularity))]

p1 <- ggplot(genre_by_popularity ,aes(x=reorder(playlist_genre,-avg_popularity),y=avg_popularity))+geom_col(aes(fill=playlist_genre))+
  geom_text(aes(label = round(avg_popularity,2)), color = "black", size = 3,vjust = -1)+
  labs(x="Playlist Genre",y="Average Popularity 0-100",
    caption="Source: tidytuesday:spotify_songs",
       title="The most popular playlist genres by average popularity")+
  theme_classic() +
  theme(axis.text = element_text(color="black"),
        axis.title = element_blank())+
  geom_hline(yintercept=mean_popularity, color="grey40", linetype=3)+
  annotate(
    "text",
    y = mean_popularity+6 ,x=6,
    label = "The\naverage\npopularity",
    vjust = 1, size = 3, color = "grey40")
p1

#########################################################
## top 20 by track popularity
top20 <- spotify[year==2020,.(song=unique(track_name)),by=track_popularity][order(desc(track_popularity))][1:20]

p2 <- ggplot(top20, aes(x = track_popularity, y = reorder(song,track_popularity), color = track_popularity)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 10, yend = song), size = 2)+
  geom_text(aes(label = track_popularity), color = "white", size = 2)+
  scale_x_continuous("", expand = c(0, 0), limits = c(0, 105), position = "top") +
  scale_color_gradientn(colors = brewer.pal(5, "RdYlBu")[-(2:4)])+
  labs(caption="Source: tidytuesday:spotify_songs",
       title="The most popular 20 songs in 2020 ")+
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color="black"),
        axis.title = element_blank())+
  geom_vline(xintercept=mean_popularity, color="grey40", linetype=3)+
  annotate(
    "text",
    x = mean_popularity+5 , y = 5.5,
    label = "The\naverage\npopularity",
    vjust = 1, size = 3, color = "grey40")+
  annotate(
    "curve",
    x =mean_popularity +5 , y = 5.5,
    xend = mean_popularity, yend = 7.5,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )
p2
#########################################################
# check the association between the tempo and danceability
# how danceability is changing based on the tempo?
# which range is the best tempo for the highest danceability?
energy_vs_danceability <- spotify[,.(track_name,playlist_genre,track_popularity,danceability,energy,liveness,valence,tempo)][order(track_popularity)]

p3 <- ggplot(energy_vs_danceability,aes(tempo,danceability))+geom_point(alpha=0.1)+
  geom_smooth(method = "loess",se=F,aes(color=playlist_genre))
p3

#########################################################
genre_by_year <- spotify[!is.na(playlist_genre),.(n_songs=.N),by=.(playlist_genre,year)][order(year)]

p4 <- ggplot(genre_by_year, aes(year, fill = playlist_genre)) +
  geom_density()+
  labs(title = 'sada')+ facet_grid(playlist_genre~.)
p4

#########################################################

top_artists <- spotify[,.(number_of_songs = .N,avg_pop=mean(track_popularity)),by=.(track_artist,year)][order(desc(year))]
top_artists <- top_artists[,.(popularity=avg_pop*number_of_songs),by=.(track_artist,year)][order(desc(year))]

top_artists_formatted <- top_artists %>%
  group_by(year) %>%
  mutate(rank = rank(-popularity)) %>%
  group_by(track_artist) %>% 
  filter(rank <=10) %>%
  ungroup()

p5 <- ggplot(top_artists_formatted, aes(rank, group = track_artist, 
                                       fill = as.factor(track_artist), color = as.factor(track_artist))) +
  geom_tile(aes(y = popularity/2,
                height = popularity,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(track_artist, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=popularity,label=popularity, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = p5 + transition_states(year, transition_length = 2, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Top 10 Artists',  
       subtitle  =  "Year : {closest_state}",
       caption  = "tidytuesday:spotify_songs")
anim

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

#########################################################
genre<- spotify[!is.na(playlist_genre),
                         .(n_song=.N,
                         avg_popularity=mean(track_popularity)),
                         by=.(playlist_genre,year)][order(desc(year))]

ggplot(genre ,aes(y=reorder(playlist_genre,avg_popularity),x=avg_popularity))+geom_col(aes(fill=playlist_genre))+
  transition_states(year, transition_length = 2, state_length = 1) + scale_y_log10()+
  labs(x="Playlist Genre",y="Number of Songs",
       caption="Source: tidytuesday:spotify_songs",
       title="The most popular playlist genres by the number of songs",
       subtitle="Year : {closest_state}")+
  theme_classic() 

#enerjisi en yuksek sarkilar yapan sarkici
energy <- spotify[,.(n_tracks=.N,
                     avg_energy=mean(energy)),
                  by=track_artist][order(desc(avg_energy))]

