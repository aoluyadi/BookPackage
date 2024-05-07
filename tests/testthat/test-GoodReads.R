test_that("check that book detail works and gives you the objects you expct from it", {
  # book_details should have 7 columns
  expect_equal(length(book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")), 7)
  # books_details should have 100 rows
  expect_equal(nrow(book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")), 100)
  # books_details should return a data frame
  expect_equal(is.data.frame(book_details("https://www.goodreads.com/list/show/1.Best_Books_Ever")), TRUE)
})

library(vdiffr)

test_that("check that book_cover plots image", {
  vdiffr::expect_doppelganger(
    title = "Hunger games book cover",
    fig = book_cover(bestbooksever_df, 1))
  })
test_that("doesn't take multiple arguments", {
  expect_error(book_cover(bestbooksever_df, 1:10))
})
