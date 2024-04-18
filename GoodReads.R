library(rvest)
goodreads <- rvest::read_html("https://www.goodreads.com/list/show/1.Best_Books_Ever")


Df_title <- goodreads |>
  html_elements("div h1") |>
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

rating_chunk <- goodreads|>
  html_elements(".minirating") |>
  html_text2()
rating_chunk

score_chunk <- goodreads|>
  html_elements(".uitext a:nth-child(1)") |>
  html_text2()
score_chuck

cover <- goodreads|>
  html_elements(".bookCover") |>
  html_text2()
cover







































































































































































































