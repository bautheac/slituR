# paste_forward_slash ##########################################################
test_that("paste_forward_slash works as expected", {
  expect_equal(paste_forward_slash("test", "test"), "test/test")
})


# make_series_of_repeated_chars ################################################
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

# percentize ###################################################################
test_that("percentize works as expected", {
  expect_equal(percentize(0.1), "10%")
  expect_equal(percentize(c(0.1, 0.05)), c("10%", "5%"))
})

# get_path_last_element ########################################################
test_that("get_path_last_element works as expected", {
  expect_equal(get_path_last_element(""), NULL)
  expect_equal(get_path_last_element("test"), "test")
  expect_equal(get_path_last_element("test/test"), "test")
  expect_equal(get_path_last_element("test/test/test.r"), "test.r")
})


# make_shiny_main_directory_path ###############################################
test_that("make_shiny_main_directory_path works as expected", {
  library(withr)

  withr::local_envvar(.new = c("SHINY_ENV" = "local"))
  path <- make_shiny_main_directory_path(local = "local", server = "server")
  expect_equal(get_path_last_element(path), "local")

  withr::local_envvar(.new = c("SHINY_ENV" = "server"))
  path <- make_shiny_main_directory_path(local = "local", server = "server")
  expect_equal(get_path_last_element(path), "server")
})
