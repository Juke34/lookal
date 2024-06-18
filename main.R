# ------ run this script ------
library(purrr)
library(tidyverse)
library(arrow)


sentence <- "gratis. Encyclopaedia: with, magnificent; % quality!"
rubbish <- "woordenboek is funny with table and chair"


processed_text <- load_text(sentence)
processed_text <- clean_text(process_text)

processed_text <- set_names(processed_text, processed_text)


further_processed <- process_text(sentence)
