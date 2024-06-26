% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GoodReads.R
\name{book_cover}
\alias{book_cover}
\title{Visualize cover image.}
\usage{
book_cover(data, book_rank)
}
\arguments{
\item{data}{The dataframe produced from using the book_details function.}

\item{book_rank}{The rank of the specific book in the dataset.}
}
\description{
Given the \code{rank} of a book, this function retrieves the the cover image and
visualizes it.
}
\examples{
Best_Books_Ever <- book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")
print(Best_Books_Ever)
book_cover(Best_Books_Ever, 2)

}
