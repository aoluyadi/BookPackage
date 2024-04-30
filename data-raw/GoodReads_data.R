## code to prepare `GoodReads_data` dataset goes here


library(rvest)
library(dplyr)
library(stringr)
goodreads <- rvest::read_html("https://www.goodreads.com/list/show/1.Best_Books_Ever")

# Extracting book details from goodreads website
Df_title <- goodreads |>
  html_elements(".gr-h1--serif") |>
  html_text2()
Df_title

book_names <- goodreads|>
  html_elements(".bookTitle span") |>
  html_text2()
book_names

rank <- goodreads|>
  html_elements(".number") |>
  html_text2()
rank

author <- goodreads|>
  html_elements(".authorName span") |>
  html_text2()
author

cover <- goodreads|>
  html_elements(".bookCover") |>
  html_attr("src")
cover

cover <- cover |>
  str_extract("\\d+i/\\d+")
cover

rating_chunk <- goodreads|>
  html_elements(".minirating") |>
  html_text()

# Cleaning rating chunk
avg_rating <- rating_chunk|>
  stringr::str_extract("\\d+\\.\\d{2}")
avg_rating

total_rating <- rating_chunk|>
  stringr::str_extract("\\d+\\.?,*\\d+\\.?,*\\d*(?=\\s*ratings)")
total_rating

# Constructing data frame
goodreads_df <- data.frame(
  `Rank` = rank,
  `Book Names` = book_names,
  `Author` = author,
  `Average Rating` = avg_rating,
  `Total Rating` = total_rating,
  `Cover ID` = cover,
  stringsAsFactors = FALSE
)


# Clean column titles
colnames(goodreads_df) <- c("Rank", "Book Names", "Author", "Average Rating", "Total Rating", "Cover ID")

# Extract series name from title column
bestbooksever_df <- goodreads_df |>
  mutate(
    Series = str_extract(book_names, "\\(.*?\\)"),
    Series = ifelse(is.na(Series), NA, str_replace_all(Series, "\\(|\\)", "")),
    `Book Names` = str_replace(book_names, "\\(.*?\\)", "")
  )

# rearrange columns
bestbooksever_df <- bestbooksever_df[, c(1:3, 7, 4:6)]
rm(goodreads_df)

usethis::use_data(bestbooksever_df, overwrite = TRUE)
