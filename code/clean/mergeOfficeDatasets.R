library(schrute) 
library(purrr)
library(tidyverse)
library(janitor)
library(rvest)
library(XML)
library(stringr)
library(tidytext)
library(googlesheets)
library(widyr)
library(psych)
library(knitr)
library(here)

# Saving schrute package data as csv.
write.csv(schrute::theoffice, here("data", "raw", "schruteData.csv"), row.names = FALSE) 

# Importing data in R environment
office <- list(temp = "", # Creating list architecture to which script outputs will be saved.

               paths = "",
               test = list("", ""), 
               data = list(schrute = "", sheets = ""))

office$paths <- dir(here("data", "raw"), pattern = "\\.csv$") # Object of paths to all .csv files in dir data/raw

for (i in seq_along(office$paths)){ # read.csv each .csv in data/raw and save it to our list
  office$data[[i]] <- read.csv(here("data", "raw", office$paths[[i]])) %>% 
    as_tibble() 
}

# Comparison of datasets

all_equal(office$data$schrute, office$data$sheets) #Checking the number of columns in each dataframe on the list

compare_df_cols(office$data$schrute, office$data$sheets) #Comparing the existence and type of a given column name between both dfs

for(i in 1:length(office$data)){ #Quick look for missing values
  office$test[[i]] <- colSums(is.na(office$data[[i]]))
}

# Merging the dfs

for(i in 1:length(office$data)){ #Creating an episode id based on Ep:Se format
  office$data[[i]] <- office$data[[i]] %>%
    unite(epID, c(season, episode), sep = ":", remove = FALSE, na.rm = FALSE)
}

office$data[[2]] <- #Renaming the columns so that column names of both dfs are uniform.
  office$data[[2]] %>%
  rename(text_w_direction = line_text, 
         character = speaker)

office$data[[3]] <- #Merging and cleaning our dfs
  left_join(distinct(select(office$data[[1]], 
                          epID, episode_name, director, writer, imdb_rating, #Creating a primary key of the schrute df within the left_join argument
                          total_votes, air_date)),
          office$data[[2]]) %>%
  select(deleted, id, episode_name, epID, season, episode, scene, character, #Reorganizing order of columns
         text_w_direction, air_date, director, writer, imdb_rating,
         total_votes) %>%
  rename(sheets_id = id) %>%
  rowid_to_column("ID") %>%
  filter(deleted == "FALSE") %>%  #So far only complete cases are for non-deleted scenes/episodes
  mutate(actions = str_extract_all(text_w_direction, "\\[.*?\\]"), #cleaning up strings
         line_text = str_trim(str_replace_all(text_w_direction, "\\[.*?\\]", ""))) %>% 
  mutate_at(vars(line_text), funs(str_replace_all(., "���","'"))) %>% 
  mutate_at(vars(character), funs(tolower)) %>% 
  mutate_at(vars(character), funs(str_trim(str_replace_all(., "\\[.*?\\]", "")))) %>% 
  mutate_at(vars(character), funs(str_replace_all(., "micheal|michel|michae$", "michael"))) %>% #addressing various spellings of michael scott
  mutate(co_written = ifelse(grepl(";", writer), "TRUE", "FALSE")) %>% #Addition of `co_written` & `co_directed` columns...
  mutate(co_directed = ifelse(grepl(";", director), "TRUE", "FALSE")) %>% #...these will come in handy later. 
  select(ID:director, co_directed, writer, co_written, imdb_rating:line_text) %>% #A not so elegant juggling act,
  select(ID:scene, line_text, character, text_w_direction:total_votes) %>% #of getting our columns in order.
  select(-c(deleted, sheets_id))

# Writing the output to a csv
office$data[[3]] %>% 
  write.csv(here("data", "clean", "combined_OfficeDataset.csv"))

