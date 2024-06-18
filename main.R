# ------ run this script ------
library(purrr)
library(tidyverse)
library(arrow)

sentence <- "gratis. Encyclopaedia: with, magnificent; % quality!"
rubbish <- "woordenboek is funny with table and chair"


process_text <- load_text(sentence)
process_text <- clean_text(process_text)
var <- map(process_text, get_countries_by_word)
var_bin <- list_c(var)

res <- tibble(strings = unlist(var_bin)) %>%
  count(strings, name = "count") %>%
  arrange(desc(count)) %>%
  as.data.frame()

colnames(res) <- c("iso", "count")

