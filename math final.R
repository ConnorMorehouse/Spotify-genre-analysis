#####
# packages #
#####
require(tidyverse)

#####
# read data in #
#####

artists <- read_csv('data_by_artist.csv')
year <- read_csv('data_by_year.csv')
influence <- read_csv('influence_data.csv')
full <- read_csv('full_music_data.csv')

#####
# functions #
#####

inside <- function(x,joined) {
  
  # get vars # 
  genre = x['genre']
  id = x['id']
  xnum = x[c(3:7,10:14)]
  xnum <- sapply(xnum, as.numeric)
  
  # make matrix
  same <- joined[joined$genre == genre,]
  same <- same[same$id != id, c(3:7,10:14)]
  
  # calc distances 
  distances <- c()
  for (i in 1:nrow(same)) {
    distances[i] <- dist(bind_rows(same[i,],xnum))
  }
  
  avg.dist <- mean(distances)
  
  rm(same)
  
  return(avg.dist)
}

outside <- function(x,joined) {
  
  # get vars # 
  genre = x['genre']
  id = x['id']
  xnum = x[c(3:7,10:14)]
  xnum <- sapply(xnum, as.numeric)
  
  # make matrix
  different <- joined[joined$genre != genre,]
  different <- different[different$id != id, c(3:7,10:14)]
  
  # calc distances 
  distances <- c()
  for (i in 1:nrow(different)) {
    distances[i] <- dist(bind_rows(different[i,],xnum))
  }
  
  avg.dist <- mean(distances)
  
  rm(different)
  
  return(avg.dist)
}

test <- head(joined,20)
test <- head(joined, 1)

test$within <- apply(test, 1, inside, joined)
view(test)

test$between <- apply(test, 1, outside, joined)
view(test)

test$diff <- test$within - test$between
view(test)

#####
# plan
#####

# answer 2: similarity question 
# develop similarity metric 
#   quantile all metrics -
#   join genre and artists -
#   euclidian distance -
#   make function to calc dist -
# are artists in the same genere more similar 
#   avg dist outside genre -
#   avg dist within genre -

#####
# similarity metric 
#####

# quantile all the data  

scaled <- data.frame(name = artists$artist_name,
                     id = artists$artist_id,
                     dance = ntile(artists$danceability, 100),
                     energy = ntile(artists$energy, 100),
                     valence = ntile(artists$valence, 100),
                     tempo = ntile(artists$tempo, 100),
                     loudness = ntile(artists$loudness, 100),
                     mode = artists$mode, # might change
                     key = artists$key, # might change 
                     acousticness = ntile(artists$acousticness, 100),
                     instrumentalness = ntile(artists$instrumentalness, 100),
                     liveness = ntile(artists$liveness, 100),
                     speechiness = ntile(artists$speechiness, 100),
                     duration = ntile(artists$duration_ms, 100),
                     popularity = ntile(artists$popularity, 100),
                     count = artists$count # probs don't use
                     )

# remove duplicates 

scaled <- scaled[!duplicated(scaled$name),]

# join scaled and genre info 

joined <- left_join(scaled, 
                    select(influence, influencer_id, influencer_main_genre),
                    by = c('id' = 'influencer_id'))
joined <- unique(joined)

joined <- left_join(joined,
                    select(influence, follower_id, follower_main_genre),
                    by = c('id' = 'follower_id'))
joined <- unique(joined)

joined$genre <- ifelse(!is.na(joined$influencer_main_genre), 
                       joined$influencer_main_genre,
                       ifelse(!is.na(joined$follower_main_genre), 
                              joined$follower_main_genre,
                              NA))

joined <- joined[, !names(joined) %in% c('follower_main_genre','influencer_main_genre','count')]

# remove artists without genre info, less than 5% 

joined <- joined[!is.na(joined$genre),]
joined <- joined[joined$genre != 'Unknown',]

# clac avg dist

# sep into same and diff genre -
# create list with dist from all artists-
# avg that -
# insert into column -

joined$within <- apply(joined, 1, inside, joined)
joined$between <- apply(joined, 1, outside, joined)
joined$diff <- joined$within - joined$between
view(joined)

diff.dist <- ggplot(joined, aes(x = diff)) +
  geom_density() +
  xlab('(within distance) - (between distance)') +
  ylab('Density') +
  ggtitle('Distribution of within - between difference') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw()
diff.dist


