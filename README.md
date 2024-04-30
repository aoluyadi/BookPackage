
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GoodReads

<!-- badges: start -->
<!-- badges: end -->

The `GoodReads` package lets you webscrape a book lists information from
[GoodReads Listopia](https://www.goodreads.com/list/tag/best). The
package alo allows you to plot the cover of a book from its given rank.

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

Hunger_Games <- book_cover(Best_Books_Everdf, 1)
```
