#' @title Construct a dataframe using a particular Best Books list from
#'   Goodreads’s Listopia.
#'
#' @description This function takes a URL from Goodreads Listopia and creates a
#' data frame that includes information about the listed books through web
#' scraping.
#'
#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_text2
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param url The url of the specific Goodreads Listopia webpage.
#'
#' @return A dataframe containing information extracted from the provided web
#'   link about the listed books.
#' * Rank: Character vector with rank of the book in the list.
#' * Book Names: Character vector with the name of the book.
#' * Series: Character vector that when a book belongs to a series, it extracts
#'  the series name  and returns NA if not.
#' * Author: Character vector that contains the names of the authors of
#' the books.
#' * Average Rating: Character vector that represents the average rating of the
#'  books.
#' * Total Rating: Character vector that represents the total number of ratings
#' for each book.
#' * Cover ID: Character vector that represents the book code needed to
#' generate a cover image using the book_cover function.
#'
#' @examples
#' Best_Books_Ever <- book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")
#' print(Best_Books_Ever)
#'
#'
#' @export
#'
book_details <- function(url) {
  goodreads <- try(rvest::read_html(url))
  # Check if the request was successful
   if (inherits(goodreads, "try-error")) {
     stop("The provided URL is not valid or reachable.")
   } else {
     message("The provided URL is valid.")
   }

  # Extracting book details from goodreads website
  Df_title <- goodreads |>
    rvest::html_elements(".gr-h1--serif") |>
    rvest::html_text2()
  Df_title

  book_names <- goodreads|>
    rvest::html_elements(".bookTitle span") |>
    rvest::html_text2()
  book_names

  rank <- goodreads |>
    rvest::html_elements(".number") |>
    rvest::html_text2()
  rank

  author <- goodreads |>
    rvest::html_elements(".authorName span") |>
    rvest::html_text2()
  author

  cover <- goodreads |>
    rvest::html_elements(".bookCover") |>
    rvest::html_attr("src")
  cover

  cover <- cover |>
    stringr::str_extract("\\d+i/\\d+")
  cover

  rating_chunk <- goodreads |>
    rvest::html_elements(".minirating") |>
    rvest::html_text()

  # Cleaning rating chunk
  avg_rating <- rating_chunk |>
    stringr::str_extract("\\d+\\.\\d{2}")
  avg_rating

  total_rating <- rating_chunk |>
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
  colnames(goodreads_df) <- c("Rank", "Book Names", "Author", "Average Rating",
                              "Total Rating", "Cover ID")

  # Extract series name from title column
  goodreads_df <- goodreads_df |>
    mutate(
      Series = str_extract(book_names, "\\(.*?\\)"),
      Series = ifelse(is.na(Series), NA, str_replace_all(Series, "\\(|\\)", "")),
      `Book Names` = str_replace(book_names, "\\(.*?\\)", "")
    )

  # rearrange columns
  goodreads_df <- goodreads_df[, c(1:3, 7, 4:6)]

  return(goodreads_df)
}


#' @title Visualize cover image.
#'
#' @description
#' Given the `rank` of a book, this function retrieves the the cover image and
#' visualizes it.
#'
#' @importFrom magick image_read
#'
#' @param data The dataframe produced from using the book_details function.
#' @param book_rank The rank of the specific book in the dataset.
#'
#' @examples
#' Best_Books_Ever <- book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")
#' print(Best_Books_Ever)
#' book_cover(Best_Books_Ever, 2)
#'
#' @export
#'
book_cover <- function(data, book_rank) {

  if (is.numeric(book_rank) == TRUE ){
    cover_id <- data$`Cover ID`[book_rank]
  } else {
    stop("Rank must be a single numeric value")
  }

  if (is.na(cover_id)) {
    stop("Invalid book rank provided. Book rank value must be greater than 0 and less than/equal to a 100")
  }

  # Constructing the cover URL
  cover_url <- paste0("https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/", cover_id, ".jpg")

  # Reading the image from the URL
  book_cover_image <- magick::image_read(cover_url)

  # Returning the book cover as an image once a book rank has been provided as a numeric value
  return(book_cover_image)
}


