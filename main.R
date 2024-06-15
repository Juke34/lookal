library("arrow")

# 'Parquet' is a columnar storage file format. 
# This function enables you to read Parquet files into R.
parquet_file <- "etymology.parquet"
etymology <- read_parquet(
  parquet_file,
  col_select = NULL,
  as_data_frame = TRUE,
  props = ParquetArrowReaderProperties$create(),
  mmap = TRUE
)

# 'country-by-languages' read a json file into a data frame
country_by_languages <- jsonlite::fromJSON("country-by-languages.json")

# change the column name of the data frame
colnames(country_by_languages) <- c("country", "lang")

# 'etymology' is a data frame that contains the etymology of words with a column lang that represent the language
# We have country_by_languages that is a data frame that contains the country and the language spoken in that country
# We would like to add the country to the etymology data frame based on the language
# Merge the two data frames based on the language column
etymology_country <- merge(etymology, country_by_languages, by = "lang")
etymology_country


# make a function to get the word based on the term_id
get_word <- function(term_id, etymology) {
  etymology[etymology$term_id == term_id, "word"]
}

# get the word for term_id 1
get_word("3Rn-54N_V5S9iGx-nUenpA", etymology)
