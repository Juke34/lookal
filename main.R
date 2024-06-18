# ------ run this script ------
library(purrr)
library(tidyverse)
library(arrow)


sentence <- "gratis. Encyclopaedia: with, magnificent; % quality!"
rubbish <- "woordenboek is funny with table and chair"


process_text <- load_text(sentence)
process_text <- clean_text(process_text)

process_text <- set_names(process_text, process_text)

var <- map(process_text, get_countries_by_word)
var_bin <- list_c(var)

res <- tibble(strings = unlist(var_bin)) %>%
  count(strings, name = "count") %>%
  arrange(desc(count)) %>%
  as.data.frame()

colnames(res) <- c("iso", "count")

# to get the words that were not counted in. not_found string
var_named <- set_names(var, map_chr(var, "iso"))
idx <- map(var, function(x) nrow(x) < 1)
not_found <- names(which(map_lgl(idx, ~ .x)))


