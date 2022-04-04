#Prepping for an exploratory analysis of the office dataset...

#Natural language 
#Writers/Directors/Actors
#Imdb rating correlations
#Story map via text analysis

#Also look to other examples of how this dataset has been manipulated
#####################################################################################################################

library(schrute) #Seemingly a package that is nothing more than a The Office dataset? #Perhaps there needs to be a...schruteR package?
library(purrr)
library(tidyverse)
library(janitor)
library(arsenal)
library(rvest)
library(XML)
library(stringr)
library(tidytext)
library(scales)
library(googlesheets)
library(igraph)
library(ggraph)
library(widyr)
library(psych)
library(kableExtra)
library(knitr)
library(plotly)
library(ggcorrplot)
library(reticulate)
library(cleanNLP)
library(packcircles)
library(patchwork)
library(reshape2)
library(ggthemes)
library(ruler)

install.packages("ruler")
install.packages("ggpstatplot")

#####################################################################################################################


schruteData <- schrute::theoffice
write.csv(schruteData, "/home/marvaenaeldo/Documents/Projects/Coding/R/officeR/shruteData.csv", row.names = FALSE) #Saving df from shruteData as .csv for the purposes of this exercise

#####################################################################################################################
#Playing around with fp and for loops saving all csv files in a directory as dfs within a list

paths <- dir(pattern = "\\.csv$") # Find all the csv files in the Office directory

data <- vector("list", length(paths)) # Here we have a for loop, which has created a new list of the correct size (create an output:1)
for (i in seq_along(paths)){ # Then creating a vector to iterate over (2) (avoid 1:length(paths) because it fails in unhappy ways if length is 0)
  data[[i]] <- read.csv(paths[[i]]) %>% # Code that is run at the end of the iteration (3)
    as_tibble() #This was added after the fact to save each csv as a tibble instead of a df within the list
}

#####################################################################################################################

all_equal(data[[1]], data[[2]]) #Checking the number of columns in each dataframe on the list
compare_df_cols(data[[1]], data[[2]]) #Comparing the existence and type of a given column name between both dfs

data[[1]] <- data[[1]] %>% #Creating and episode id based on Ep:Se formation for data[1]
  unite(epID, c(season, episode), sep = ":", remove = FALSE, na.rm = FALSE) 

data[[2]] <- data[[2]] %>%
  unite(epID, c(season, episode), sep = ":", remove = FALSE, na.rm = FALSE) %>% #Creating and episode id based on Ep:Se formation for data[2]
  rename( #Renaming the columns on data[2] so that column names of both dfs are uniform.
    text_w_direction = line_text,
    character = speaker)

epID <- data[[2]] %>% #Pulling epID from data[["sheetsData"]], which will be used to write over epID in data[["epiKey"]]
  select(epID) %>%
  distinct()

data[[3]] <- data[[1]] %>% #Creating a primary key for Office Episodes
  unite(epID, c(season, episode), sep = ":", remove = FALSE, na.rm = FALSE) %>%
  select(epID, episode_name, director, writer, imdb_rating,
         total_votes, air_date) %>%
  distinct()

data[[3]]["epID"] <- epID

data[[4]] <- left_join(data[[3]], data[[2]]) %>% #Populating data[2] with episode information using the left_join() function.
  select(deleted, id, episode_name, epID, season, episode, scene, character, #Reorganizing order of columns in data[4]
         text_w_direction, air_date, director, writer, imdb_rating,
         total_votes) %>%
  rename(sheets_id = id) %>%
  rowid_to_column("ID") 

colSums(is.na(data[[4]])) #Checking for missing data

data[[5]] <- data[[4]] %>% # Cleaning the dataset
  filter(deleted == "FALSE") %>% 
  mutate(actions = str_extract_all(text_w_direction, "\\[.*?\\]"),
         line_text = str_trim(str_replace_all(text_w_direction, "\\[.*?\\]", ""))) %>% 
  mutate_at(vars(line_text), funs(str_replace_all(., "���","'"))) %>% 
  mutate_at(vars(character), funs(tolower)) %>% 
  mutate_at(vars(character), funs(str_trim(str_replace_all(., "\\[.*?\\]", "")))) %>% 
  mutate_at(vars(character), funs(str_replace_all(., "micheal|michel|michae$", "michael"))) %>%
  mutate(co_written = ifelse(grepl(";", writer), "TRUE", "FALSE")) %>%
  mutate(co_directed = ifelse(grepl(";", director), "TRUE", "FALSE")) %>%
  select(ID:director, co_directed, writer, co_written, imdb_rating:line_text) %>%
  select(ID:scene, line_text, character, text_w_direction:total_votes)

names(data)[1:5] <- c("Schrute_Data", "Sheets_Data", "epiKey", "left_joined", "dataClean") #This should be names first, it is generally better practice to be in the habit of naming your objects first.

# Additional Data Wrangling - Tidying the writer and director columns
# It is likely that this should go into it's own table.

creator_test <- data$dataClean %>%
  separate(director, c("director_1", "director_2"), ";") %>%
  gather(director_code, director, director_1:director_2) %>%
  filter(!is.na(director)) %>%
  separate(writer, c("writer_1", "writer_2", "writer_3"), ";") %>%
  gather(writer_code, writer, writer_1:writer_3) %>%
  filter(!is.na(writer)) %>%
  arrange(ID, season, episode, scene)

# Actor Table

# Creating a list of the main 
actors <- read_html("https://en.wikipedia.org/wiki/List_of_The_Office_(American_TV_series)_characters")

actor_table <-
  actors %>%
  html_nodes("td > a") %>%
  html_text() %>% 
  as_tibble() %>%
  mutate(actor = value) %>%
  mutate_at(c("actor"), list(lead), n = 1) %>%
  filter(row_number() %% 2 == 1) %>%
  rename(character = value) %>%
  arrange(character)


imdb_fullcreds <- read_html("https://www.imdb.com/title/tt0386676/fullcredits")

imdb_fullcreds %>% # Something else to work with, a bit more comprehensive than the wiki page
  html_nodes("#fullcredits_content a") %>%
  html_text()



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
