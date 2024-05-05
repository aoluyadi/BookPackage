
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GoodReads

<!-- badges: start -->

[![R-CMD-check](https://github.com/aoluyadi/BookPackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aoluyadi/BookPackage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `GoodReads` package lets you webscrape a book lists information from
[GoodReads Listopia](https://www.goodreads.com/list/tag/best). The
package also allows you to plot the cover of a book from its given rank.

## Installation

You can install the development version of the GoodReads package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aoluyadi/BookPackage")
```

## Usage

Using a url from Goodread’s Listopia of Best Books Lists, the package
webscrapes information for the first 100 books.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(GoodReads)

Best_Books_Everdf <- book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")
#> The provided URL is valid.

book_cover(Best_Books_Everdf, 1)
```

<img src="man/figures/README-example-1.png" width="100%" />
