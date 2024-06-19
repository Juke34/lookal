# --- parameters ----
## --- lang file ---
tsv_lang_file <- "data/country-by-languages.tsv"
# json_lang_file <- "data/countries.json"
## --- etymology file ---
#csv_etymology_file <- "data/test_etymology_10000.csv"
parquet_etymology_file <- "data/etymology.parquet"
                    

# ---- File for functions to process the input -----

load_text <- function(sentence, pdf = FALSE, pdf_input){
  # function to read in pdf file or text in a string. Both are checked for whether 
  # the character vector is longer than 1 (due to quotation marks) and are then merged.
  if(pdf == TRUE){
    sentence <- pdftools::pdf_text(pdf_input)
  }
  if(length(sentence) > 1){
    updated_string <- c()
    for (ii in (1:length(sentence))) {
      updated_string <- str_c(updated_string, sentence[ii], sep = " ")
      
    }
    sentence <- updated_string
  }
  sentence
}


clean_text <- function(sentence){
  # this function removes all punctuation and digits, to be left with the words.
  # strings are split by space
  # all letters are converted to lowercase
  # the strings are converted to a list
  # empty character strings are excluded  
  clean_sentence <- str_replace_all(sentence, pattern = "[[:punct:]]|[[:digit:]]", replacement = "")
  separate_sentence <- str_split_1(clean_sentence, pattern = "[[:space:]]") 
  separate_sentence <- tolower(separate_sentence)
  sentence_list <- as.list(separate_sentence)
  sentence_list <- sentence_list[nchar(sentence_list) > 0]
}


#-------- Get ethymology data from parquet file and csv file --------

##-------- Using Parquet input file ------------

# 'Parquet' is a columnar storage file format. 
# This function enables you to read Parquet files into R.
etymology <- arrow::read_parquet(
  parquet_etymology_file,
  col_select = NULL,
  as_data_frame = TRUE,
  props = ParquetArrowReaderProperties$create(),
  mmap = TRUE
)

## ------ Using CSV input file ------------

# Get content into a data frame from CSV file
#etymology <- read.csv(csv_etymology_file,
                      #header = TRUE, sep = ",")

#-------- Get country-by-language data ------------

## -----  from json file ----
#country_by_languages <- jsonlite::fromJSON(json_lang_file)
#country_by_languages <- as.data.frame(country_by_languages)

## -----  from tsv file ----
country_by_languages <- read.csv(tsv_lang_file,
                                 row.names = NULL,
                                 header = TRUE, sep = "\t")
colnames(country_by_languages) <- c("country", "ISO3", "lang")


get_countries_by_word <- function(word, et = etymology, co = country_by_languages){
  # get a vector of all the countries that map with term (portmanteau) in the etymology data table
  country_list <- et[et$term == word, "lang"] %>% unique()
  
  # select the iso based on a list of languages
  result <- co %>%
    filter(lang %in% country_list ) %>%
    select("ISO3") %>% 
    unique() 
  
  return(result)
}

process_text <- function(sentence = "", et = etymology, co = country_by_languages, not_fo = TRUE){
  # function to process the input text
  # the text is cleaned
  # the cleaned text is then processed to get the countries that map with the words in the text
  # the countries are then returned as a list
  # now it only returns the results
  loaded_text <- load_text(sentence)
  cleaned_text <- clean_text(loaded_text)
  var <- map(cleaned_text, get_countries_by_word)
  var_bin <- list_c(var)
  
  res <- tibble(strings = unlist(var_bin)) %>%
    count(strings, name = "count") %>%
    arrange(desc(count)) %>%
    as.data.frame()
  colnames(res) <- c("ISO3", "count")
  
  # keep track of words that do not match any country name 
  if(not_fo == TRUE){
    idx <- map(var, function(x) nrow(x) < 1)
    not_found <- names(which(map_lgl(idx, ~ .x)))
  }
  
  return(res)
 # return(not_found)
}


world_sf <- read_sf("data/world_shape/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_sf <- world_sf %>%  mutate(Count = 0)
