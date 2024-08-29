# paste_forward_slash ####
test_that("paste_forward_slash works as expected", {
  expect_equal(paste_forward_slash("test", "test"), "test/test")
})


# make_series_of_repeated_chars ####
test_that("make_series_of_repeated_chars works as expected", {
  chars <- "lllrrrrllcc"
  expect_equal(
    make_series_of_repeated_chars(list(l = 3L, r = 4L, l = 2L, c = 2L)), chars
    )
  chars <- "llrrrrlllcc"
  expect_equal(
    make_series_of_repeated_chars(list(l = 2L, r = 4L, l = 3L, c = 2L)), chars
    )
})
