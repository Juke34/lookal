# --- parameters ----
## --- lang file ---
tsv_lang_file <- "data/country-by-languages.tsv"
# json_lang_file <- "data/countries.json"
## --- etymology file ---
csv_etymology_file <- "data/test_etymology_10000.csv"
#parquet_etymology_file <- "data/etymology.parquet"
                    

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
#etymology <- arrow::read_parquet(
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



# -------- For Sankey plot -----

get_languages_nodes_by_word <- function(word, et = etymology){
  
  # select the langs based on a list of languages_nodes
  result <- et %>%
    filter(term == word) %>%
    filter(reltype == "inherited_from") %>%
    select("lang", "related_lang")
  
  return(result)
}


# create two data frames and return both into a single list.
# first dataframe is called nodes
# second dataset is called links
all_lang_by_words <- function(sentence,  et = etymology){
  loaded_text <- load_text(sentence)
  cleaned_text <- clean_text(loaded_text)
  list_langs <- map(cleaned_text, get_languages_nodes_by_word)
  # bind list of df together
  var_bin <- list_rbind(list_langs)
  # Group by the two columns and count occurrences
  var_bin <- var_bin %>% 
    group_by(lang, related_lang) %>%  
    summarise(count = n(), .groups = 'drop')
  
  # create index of country 
  language_level <- c(unique(var_bin$lang), unique(var_bin$related_lang))
  languages_nodes <- as.data.frame(language_level)
  languages_nodes$number <- rownames(languages_nodes)
  
  
  # Merge with languages_nodes dataframe that has a unique number per language
  lang_merge <- merge(var_bin, languages_nodes, by.x = "lang", by.y = "language_level")
  colnames(lang_merge) <- c("lang1", "related_lang1", "count", "lang")
  related_lang_merge <- merge(lang_merge, languages_nodes, by.x = "related_lang1", by.y = "language_level")
  colnames(related_lang_merge) <- c("lang1", "related_lang1", "count", "lang", "related_lang")
  just_numbers <- related_lang_merge[, 3:5]
  just_numbers$lang <- as.integer(just_numbers$lang)
  just_numbers$related_lang <- as.integer(just_numbers$related_lang)
  just_numbers$lang <- just_numbers$lang - 1
  just_numbers$related_lang <- just_numbers$related_lang - 1
  
  df_list <- list(just_numbers, languages_nodes)
  names(df_list) <- c("links", "nodes")
  
  return(df_list)
}

# Return a sankey plot 
get_sankey_plot <- function(sentence,  et = etymology){
  df_list <- all_lang_by_words(sentence, et)
  p <- sankeyNetwork(Links = df_list$links, 
                   Nodes = df_list$nodes, 
                   Source = "lang",
                   Target = "related_lang", 
                   Value = "count", 
                   NodeID = "language_level",
                   #    units = "counted", 
                   fontSize = 12, 
                   nodeWidth = 30)
  return(p)
}

