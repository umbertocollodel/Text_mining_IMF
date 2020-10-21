library(stringr)
library(purrr)
library(tidytext)
library(tidyverse)
library(pdftools)



# Absolute path maintained for pure explanation purpose. Change to location of file name in the hard disk.

# Proposition of netting for global depreciations (terms present in our keyword category, but
# referring to other countries e.g. U.S.)  -----

# First example:

file <- pdf_text("~/Desktop/CHN_2006-07-28_Article IV.pdf") %>% 
  str_split("\n") 



# files=load("../Betin_Collodel/2. Text mining IMF_data/datasets/corpus/corpus.RData")
# 
# file=x[[57]]$`FRA_2010-07-23_Article IV`
# export(tibble(file),"FRA2010.txt")

# Collapse the corpus:

file_collapsed <- file %>%
  map(~ paste(.x, collapse = " ")) %>%  # first, collapse the rows of every page
  paste(collapse = "") # then, collapse the different pages

# Tokenisation:

# Without addititional criterion

# keywords=c("unemployment")
# 
# list(sentence = tibble(doc = file_collapsed) %>% 
#        unnest_tokens(word, doc, token = "sentences") %>% 
#        filter(grepl(paste(keywords,collapse = "|"),word)) %>% 
#        data.frame(),
#      count = tibble(doc = file_collapsed) %>% 
#        unnest_tokens(word, doc, token = "sentences") %>% 
#        filter(grepl(paste(keywords,collapse = "|"),word)) %>% 
#        count() 
# )

list(sentence = tibble(doc = file_collapsed) %>% 
  unnest_tokens(word, doc, token = "sentences") %>% 
  filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word)) %>% 
  data.frame(),
  count = tibble(doc = file_collapsed) %>% 
    unnest_tokens(word, doc, token = "sentences") %>% 
    filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word)) %>% 
    count() 
)

# With additional criterion

list(sentence = tibble(doc = file_collapsed) %>% 
  unnest_tokens(word, doc, token = "sentences") %>% 
  filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word) & !grepl("sharp depreciation in the u.s. dollar", word)) %>% 
  data.frame(),
  count=tibble(doc = file_collapsed) %>% 
    unnest_tokens(word, doc, token = "sentences") %>% 
    filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word) & !grepl("sharp depreciation in the u.s. dollar", word)) %>% 
    count()
)

# Second example:

run_example_proposition <- function(absolute_path) {
  
  file <- pdf_text(absolute_path) %>% 
    str_split("\n") 
  
  # Collapse the corpus:
  
  file_collapsed <- file %>%
    map(~ paste(.x, collapse = " ")) %>%  # first, collapse the rows of every page
    paste(collapse = "") # then, collapse the different pages
  
  
  # Tokenisation:
  
  # Without addititional criterion
  
  without <- list(sentence = tibble(doc = file_collapsed) %>% 
    unnest_tokens(word, doc, token = "sentences") %>% 
    filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word)) %>% 
      data.frame(),
    count = tibble(doc = file_collapsed) %>% 
      unnest_tokens(word, doc, token = "sentences") %>% 
      filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word)) %>% 
      data.frame() %>% 
      count()
  )
  
  # With additional criterion
  
  with <- list(sentence = tibble(doc = file_collapsed) %>% 
    unnest_tokens(word, doc, token = "sentences") %>% 
    filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word) & !grepl("while a sharp depreciation.*would", word)),
    count = tibble(doc = file_collapsed) %>% 
      unnest_tokens(word, doc, token = "sentences") %>% 
      filter(grepl(paste(key_words_crisis()[["Currency_crisis"]],collapse = "|"),word) & !grepl("while a sharp depreciation.*would", word)) %>% 
      count()
  )
  
  return(list(without = without, with = with))
}



run_example_proposition("~/Desktop/275711.PDF")


# Another look at the outcome with same cleaning as in clean_file.R to understand what is happening with endings, number of characters and footnotes.
# Number of characters: ok!
# Endings: fucked

file <- pdf_text("~/Desktop/CHN_2006-07-12_Article IV.pdf") %>% 
  str_split("\n") 

# Collapse the corpus:

file_collapsed <- file %>%
  map(~ paste(.x, collapse = " ")) %>%  # first, collapse the rows of every page
  paste(collapse = "") # then, collapse the different pages

str_view()
tibble(doc = file_collapsed) %>% 
  mutate(doc = str_replace_all(doc, "\\s+"," ")) %>%
  data.frame()
  unnest_tokens(word, doc, token = "sentences") %>% 
  mutate(is = grepl("major markets also",word)) %>% 
  filter(is == "TRUE")


# Are we counting the tf in the correct way? ----
# We have to count number of sentences, not number of characters.
  
tibble(doc = file_collapsed) %>% 
    unnest_tokens(word, doc, token = "sentences") %>% 
    nrow()


  
  