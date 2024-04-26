library(rvest)
library(dplyr)
library(stringr)

book_details <- function(url) {
  goodreads <- read_html(url)

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
    `Cover` = cover,
    `Book Names` = book_names,
    `Author` = author,
    `Average Rating` = avg_rating,
    `Total Rating` = total_rating,
    stringsAsFactors = FALSE
  )

  # Clean column titles
  colnames(goodreads_df) <- c("Rank", "Cover", "Book Names", "Author", "Average Rating", "Total Rating")

  # Extract series name from title column
  goodreads_df <- goodreads_df |>
    mutate(
      Series = str_extract(book_names, "\\(.*?\\)"),
      Series = ifelse(is.na(Series), NA, str_replace_all(Series, "\\(|\\)", "")),
      `Book Names` = str_replace(book_names, "\\(.*?\\)", "")
    ) |>
    select(Rank, Cover, `Book Names`, Series, Author, `Average Rating`, `Total Rating`)

  return(goodreads_df)
}



cover_urls <- list()

for (i in 1:length(cover)) {
  urls <- paste0("https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/", cover[[i]], ".jpg")
  cover_urls[[i]] <- urls
}

# Iterating over the list of urls to get a list of image covers
images_list <- list()

for (i in seq_along(cover_urls)) {
  # Now 'images_list' contains all the images read from the URLs
  images_list[[i]] <- image_read(cover_urls[[i]])
  print(images_list)
}

library(magick)
imgs = image_read('https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1628267712i/24178.jpg')
print(imgs)







