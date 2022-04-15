
# Exploratory Analysis

# Visualization 

data$dataClean %>% 
  select(episode_name, epID, season, episode, air_date, director, writer, imdb_rating, total_votes) %>%
  distinct() %>%
  mutate_at(c("epID", "season"), as.factor) %>%
  ggplot(aes(episode, imdb_rating, color = season)) + 
  geom_jitter() + 
  geom_smooth(se = FALSE)

data$dataClean %>% 
  select(episode_name, epID, season, episode, air_date, director, writer, imdb_rating, total_votes) %>%
  distinct() %>%
  mutate_at(c("epID", "season"), as.factor) %>%
  ggplot(aes(episode, imdb_rating, colour = season)) + 
  geom_jitter() + 
  geom_smooth() + 
  facet_wrap(~ season, nrow = 3)

# Tracking imbd rating, and finding best and worst episodes as outliers.
data$dataClean %>% 
  select(episode_name, epID, season, episode, air_date, director, writer, imdb_rating, total_votes) %>%
  distinct() %>%
  mutate_at(c("epID", "season"), as.factor) %>%
  ggplot(mapping = aes(x = season, y = imdb_rating)) + 
  geom_boxplot() + 
  theme_clean()

creator_test %>%
  select(epID, episode_name, season, episode, director, imdb_rating, total_votes) %>%
  distinct() %>%
  group_by(director) %>%
  summarise(count = n()) %>%
  filter(count >= 5) %>%
  ggplot(mapping = aes(x = reorder(director, (-count)), y = count)) + 
  geom_bar(stat = 'identity') + 
  theme_clean() +
  coord_flip()

creator_test %>%
  select(epID, episode_name, season, episode, writer, imdb_rating, total_votes) %>%
  distinct() %>%
  group_by(writer) %>%
  summarise(count = n()) %>%
  filter(count >= 5) %>%
  ggplot(mapping = aes(x = reorder(writer, (-count)), y = count)) + 
  geom_bar(stat = 'identity') +
  theme_clean() + 
  coord_flip()

# Questions: 

# Is there a specific theme, preferred plot/character arch that is evident in the writing and/or directing style of specific writers/directors.
# Who is the main character of the series.
# Who is the main character of a given season.
# Who is the main character of a given episode. 
# What sense can be made from what characters say in private relative to what they say in public. 

# Tasks

# Create table of office characters and the actors who play them.
# Make table of episodes associated with each writer and director.
# Look into cooccur and visNetwork packages.
# Look into making timelines
# Create list of fan theories. 
# Find other rating systems metrics by which to score episodes etc.
# Tidy up character dialogue (separate lines spoken by multiple characters)
# Tidy up character dialogue (some characters are first and last name, mispellings etc)

data$dataClean %>%
  group_by(character) %>%
  summarise(count = n()) %>%
  filter(count > 400) %>%
  View()

