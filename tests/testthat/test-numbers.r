# floor_to_nearest_multiple ####
test_that("floor_to_nearest_multiple works as expected", {
  expect_equal(floor_to_nearest_multiple(25L, 6L), 24L)
  expect_equal(floor_to_nearest_multiple(0L, 6L), 0L)
})

# floor_year_to_nearest_decade ####
test_that("floor_year_to_nearest_decade works as expected", {
  expect_equal(floor_year_to_nearest_decade(1998L), 1990L)
  expect_equal(floor_year_to_nearest_decade(2004L), 2000L)
})

# remove_decade_from_year ####
test_that("remove_decade_from_year works as expected", {
  expect_equal(remove_decade_from_year(1998L),8L)
  expect_equal(remove_decade_from_year(2004L), 4L)
})
