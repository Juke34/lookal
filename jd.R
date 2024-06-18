library("arrow")
library("jsonlite")
library("tidyverse")

#--------functions ------------ 

# make a function to get the word based on the term_id
get_word <- function(term_id, etymology) {
  etymology[etymology$term_id == term_id, "word"]
}

# The country_by_languages data frame has a vector in the lang column
# I would like to inflate the vectors into multiple rows avoiding mutate and unnest functions
# Define a function to expand the data frame
expand_lang_column <- function(df) {
  # Create a list to store the results
  result <- list()
  
  # Iterate through each row of the data frame
  for (i in 1:nrow(df)) {
    # Extract the country and languages for the current row
    country <- df$country[i]
    languages <- df$lang[[i]]
    
    # Create a temporary data frame with replicated country and expanded languages
    temp_df <- data.frame(
      country = rep(country, length(languages)),
      lang = languages
    )
    
    # Append the temporary data frame to the result list
    result[[i]] <- temp_df
  }
  
  # Combine all the temporary data frames into a single data frame
  do.call(rbind, result)
}

#-------- Get ethymology data from parquet file and csv file --------

##-------- Using Parquet input file ------------

# 'Parquet' is a columnar storage file format. 
# This function enables you to read Parquet files into R.
#parquet_file <- "data/etymology.parquet"
#etymology <- read_parquet(
#  parquet_file,
#  col_select = NULL,
#  as_data_frame = TRUE,
#  props = ParquetArrowReaderProperties$create(),
#  mmap = TRUE
#)

## ------ Using CSV input file ------------

# Get content into a data frame from CSV file
json_file <- "data/test_etymology_10000.csv"
etymology <- read.csv(json_file,
                      header = TRUE, sep = ",")

#-------- Get country-by-language data ------------

## -----  from json file ----
# 'country-by-languages' read a json file into a data frame
country_by_languages <- jsonlite::fromJSON("data/countries.json")
country_by_languages <- as.data.frame(country_by_languages)
View(country_by_languages)

## -----  from tsv file ----
lang_file <- "data/country-by-languages.tsv"
country_by_languages <- read.csv(lang_file,
                                 row.names = NULL,
                                 header = TRUE, sep = "\t")
colnames(country_by_languages) <- c("country", "iso", "lang")

#-------- Clean the data ------------
#country_by_languages %>% 
#  mutate(languages = str_split(languages, ",\\s*")) %>%
#  unnest(language) 

# change the column name of the data frame
#colnames(country_by_languages) <- c("country", "lang")



# Apply the function to the data frame
#country_by_languages <- expand_lang_column(country_by_languages)

#-------- Merge etymology and country_by_languages ------------

# Merge the two data frames based on the values of the lang column in etymology dataframe en all values of the vector in the lang column in country_by_languages
#etymology_country <- merge(etymology, country_by_languages, by = "lang")

# from word it will catch the 
get_countries_by_word<-function(word, etymology, country_by_languages){
  # get a vector of all the countries that map with term (portmanteau) in the etymology data table
  country_list <- etymology[etymology$term == word, "lang"] %>% unique()
  
  # select the iso based on a list of languages
  country_by_languages %>%
      filter(lang %in% country_list ) %>%
      select("iso") %>% 
      unique() 
}

get_countries_by_word("portmanteau", etymology, country_by_languages)









# get a vector of all the countries that map with term (portmanteau) in the etymology data table
country_list <- etymology[etymology$term == "portmanteau", "lang"] %>% unique()

#lang <- etymology %>%
#  filter(term == "portmanteau") %>%
#x  select(lang) %>% as.list() %>% unlist()
#  unique() %>% 


# select the iso based on a list of languages
country_by_languages %>%
  filter(lang %in% country_list ) %>%
  select("iso") %>% 
  unique() 
  
