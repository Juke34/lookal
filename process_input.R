# --- parameters ----
## --- lang file ---
tsv_lang_file <- "data/country-by-languages.tsv"
# json_lang_file <- "data/countries.json"
## --- etymology file ---
csv_etymology_file <- "data/test_etymology_10000.csv"
# parquet_etymology_file <- "data/etymology.parquet"

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
  # the strings are converted to a list
  # empty character strings are excluded  
  clean_sentence <- str_replace_all(sentence, pattern = "[[:punct:]]|[[:digit:]]", replacement = "")
  separate_sentence <- str_split_1(clean_sentence, pattern = "[[:space:]]") 
  sentence_list <- as.list(separate_sentence)
  sentence_list <- sentence_list[nchar(sentence_list) > 0]
}


#-------- Get ethymology data from parquet file and csv file --------

##-------- Using Parquet input file ------------

# 'Parquet' is a columnar storage file format. 
# This function enables you to read Parquet files into R.
#etymology <- read_parquet(
#  parquet_etymology_file,
#  col_select = NULL,
#  as_data_frame = TRUE,
#  props = ParquetArrowReaderProperties$create(),
#  mmap = TRUE
#)

## ------ Using CSV input file ------------

# Get content into a data frame from CSV file
etymology <- read.csv(csv_etymology_file,
                      header = TRUE, sep = ",")

#-------- Get country-by-language data ------------

## -----  from json file ----
#country_by_languages <- jsonlite::fromJSON(json_lang_file)
#country_by_languages <- as.data.frame(country_by_languages)

## -----  from tsv file ----
country_by_languages <- read.csv(tsv_lang_file,
                                 row.names = NULL,
                                 header = TRUE, sep = "\t")
colnames(country_by_languages) <- c("country", "iso", "lang")


get_countries_by_word <- function(word, etymology, country_by_languages){
  # get a vector of all the countries that map with term (portmanteau) in the etymology data table
  country_list <- etymology[etymology$term == word, "lang"] %>% unique()
  
  # select the iso based on a list of languages
  country_by_languages %>%
    filter(lang %in% country_list ) %>%
    select("iso") %>% 
    unique() 
}

