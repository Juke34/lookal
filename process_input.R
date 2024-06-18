# File for functions to process the input


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