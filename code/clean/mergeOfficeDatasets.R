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
library(here)

#####################################################################################################################

temp <- schrute::theoffice
write.csv(temp, here("data", "raw", "schruteData.csv")) #Saving df from shruteData as .csv for the purposes of this exercise

#####################################################################################################################
#Playing around with fp and for loops saving all csv files in a directory as dfs within a list

paths <- dir(here("data", "raw"), pattern = "\\.csv$") # Find all the csv files in the Office directory


data <- vector("list", length(paths)) # Here we have a for loop, which has created a new list of the correct size (create an output:1)
for (i in seq_along(paths)){ # Then creating a vector to iterate over (2) (avoid 1:length(paths) because it fails in unhappy ways if length is 0)
  data[[i]] <- read.csv(here("data", "raw", paths[[i]])) %>% # Code that is run at the end of the iteration (3)
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

data[[5]] %>%
  write.csv(here("data", "clean", "combined_OfficeDataset.csv"))
