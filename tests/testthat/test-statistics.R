# significance ####
test_that("significance works as expected", {
  expect_equal(significance(0.10), "")
  expect_equal(significance(0.075), "*")
  expect_equal(significance(0.025), "**")
  expect_equal(significance(0.005), "***")
  expect_equal(
    significance(c(0.10, 0.075, 0.025, 0.005)), c("", "*", "**", "***")
    )
})
