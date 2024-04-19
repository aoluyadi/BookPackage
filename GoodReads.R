library(rvest)
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
  `Cover` = cover,
  `Book Names` = book_names,
  `Author` = author,
  `Average Rating` = avg_rating,
  `Total Rating` = total_rating,
  stringsAsFactors = FALSE
)

# Clean colunm titles
colnames(goodreads_df) <- c("Rank", "Cover", "Book Names", "Author", "Average Rating", "Total Rating")


































































































































































































