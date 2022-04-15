---
title: "Merging two Popular Office (TV/US) Datasets"
author: "Kyle Simpson"
date: "4/10/2022"
output: github_document
---

There are already some good examples of how text mining can be applied to transcripts
of the US version of 'The Office'.  In these examples, there appear to be two popular
datasets that people refer to. One from the `schrute` package, and another which is 
scraped from a googlesheets directory.  The former has one major short coming in that
it does not contain information which identifies which scene a line of dialog is 
spoken in.

This project merges another popular 'The Office' dataset so that information pertaining
to 'scene number' is included.

The following packages will be used to clean and merge these datasets: 

```r
library(schrute) 
library(purrr)
library(tidyverse)
library(janitor)
library(rvest)
library(XML)
library(stringr)
library(tidytext)
library(googlesheets) #Ideally the sheets dataset would have been 
library(widyr)
library(psych)
library(knitr)
library(here)
```

Normally the dataset would be pulled from the package and saved as an object in the R
environment. However, in this case we'll export it out of the R environment as a csv 
file for the purpose of playing around with a write.csv specific for loop.

```r
# Saving schrute package data as csv.
write.csv(schrute::theoffice, here("data", "raw", "schruteData.csv"), row.names = FALSE) 
```

There's probably a more clever way of setting this up with code, especially if we don't 
know how many csv files we will be pulling from the directory.  In this case we do, so 
we can build our list by hand, and then populate the data portion of it with our
for loop.

```r
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
```

We can see that there is thankfully no cases of missingness in any of the columns of either
of the datasets. 

```r
for(i in 1:length(office$data)){ #Quick look for missing values
  office$test[[i]] <- colSums(is.na(office$data[[i]]))
}
office$test[(1:2)]
#> [[1]]
#>         index             ason          episode     episode_name         director           writer        character            
#>             0                0                0                0                0                0                0                                
#>          text text_w_direction      imdb_rating      total_votes         air_date 
#>             0                0                0                0                0 
#>
#> [[2]]
#>            id              son          episode            scene        line_text        character          deleted 
#>             0                0                0                0                0                0                0 
```

A quick comparison of the number of columns in each dataset shows that the schrute dataset has 12 relative to the 
the 7 columns which make up the googlesheets sourced dataset.  Moreover, using `is.na` in combination with 
`colSums` reveals that there are 5 variables in the sheets dataset which do not appear in the schrute dataset, 
whereas there are 10 variables from the schrute dataset which do not appear in the googlesheets dataset.

```r
all_equal(office$data$schrute, office$data$sheets) #Checking the number of columns in each dataframe on the list
#> Different number of columns: 12 vs 7.

colSums(is.na(compare_df_cols(office$data$schrute, office$data$sheets)))
#>  column_name office$data$schrute  office$data$sheets 
#>            0                   5                  10 
```

We can pipe the output of `compare_df_cols` into a kable to get a closer look of what's going on. 
In doing so, we can see that 'deleted' and 'scene' are columns from the googlesheets dataset
that we'd like to merge with the schrute dataset.  It also appears that inconsistent naming
of variables may be an issue (e.g. character vs speaker; text vs line_text).

```r
compare_df_cols(office$data$schrute, office$data$sheets) %>% #Comparing the existence and type of a given column name between both dfs
  as_tibble() %>%
  rename(col_name = 1, schrute_data = 2, sheets_data = 3) %>%
  kable()
```

|col_name         |schrute_data |sheets_data |
|:----------------|:------------|:-----------|
|air_date         |character    |NA          |
|character        |character    |NA          |
|deleted          |NA           |logical     |
|director         |character    |NA          |
|episode          |integer      |integer     |
|episode_name     |character    |NA          |
|id               |NA           |integer     |
|imdb_rating      |numeric      |NA          |
|index            |integer      |NA          |
|line_text        |NA           |character   |
|scene            |NA           |integer     |
|season           |integer      |integer     |
|speaker          |NA           |character   |
|text             |character    |NA          |
|text_w_direction |character    |NA          |
|total_votes      |integer      |NA          |
|writer           |character    |NA          |

Merging the two datasets is now just a matter of: Creating an episode ID (Se:Ep); 
Ensuring all variables are named consistently across datasets; Identifying the
primary key and merging datasets with `left_join` function; Cleaning dirty strings.

Creating a new episode ID by uniting the season and episode columns.  This will be helpful in
creating the primary key which will be used for merging the datasets via the `left_join`
function.  This needs to happen in both datasets, so a for loop can be run across the 
portion of the office list which contains them. 

```r
# Merging the dfs

for(i in 1:length(office$data)){ #Creating an episode id based on Ep:Se format
  office$data[[i]] <- office$data[[i]] %>%
    unite(epID, c(season, episode), sep = ":", remove = FALSE, na.rm = FALSE)
}
```

Ensuring all variables are named consistently across datasets.
```r
office$data[[2]] <- #Renaming the columns so that column names of both dfs are uniform.
  office$data[[2]] %>%
  rename(text_w_direction = line_text, 
         character = speaker)
```

Thanks to pipes, the creation of the primary key, the merging join, and the cleaning of any 
dirty strings can all be done at the same time. The resulting output is a merged dataset 
which is 'clean' but not 'tidy'.  We can call it clean because there are no missing values
and strings have been formatted in a uniform way.  We can not call it tidy because although 
every variable is a column, it is not the case that every observation is it's own row thanks 
to the 'Writer' and 'Director' columns. This will need to be addressed if analysis which 
focuses on the show's writers and/or directors is conducted.  To pre-empt this 'co-written' and 
'co-directed' columns will also be corrected which will contain only the values TRUE or FALSE. 

```r
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
```

Writing the clean merged dataset as a csv file to the data/clean directory of the project file. 
```r
office$data[[3]] %>% 
  write.csv(here("data", "clean", "combined_OfficeDataset.csv"))
```

