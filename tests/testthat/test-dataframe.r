# invert_columns_on_condition #########################################################

## main ########################################################################
test_that(
  "invert_columns_on_condition defaults work correctly", {

    expect_equal(
      invert_columns_on_condition(data.frame(a = c(3L, 2L, 1L), b = c(1L, 2L, 3L), c = c(1L, 2L, 3L)), "a", "b"),
      data.frame(a = c(1L, 2L, 1L), b = c(3L, 2L, 3L), c = c(1L, 2L, 3L))
    )
    expect_equal(
      invert_columns_on_condition(data.frame(a = c(6L, 5L, 4L), b = c(1L, 2L, 3L), c = c(1L, 2L, 3L)), "a", "b"),
      data.frame(a = c(1L, 2L, 3L), b = c(6L, 5L, 4L), c = c(1L, 2L, 3L))
    )

    expect_equal(
      invert_columns_on_condition(data.frame(a = c("a", "b", "c"), b = c("c", "b", "a"), c = c(1L, 2L, 3L)), "a", "b"),
      data.frame(a = c("a", "b", "a"), b = c("c", "b", "c"), c = c(1L, 2L, 3L))
    )

    expect_equal(
      invert_columns_on_condition(data.frame(a = c("2025-01-08", "2025-01-09", "2025-01-10"), b = c("2025-01-10", "2025-01-09", "2025-01-08"), c = c(1L, 2L, 3L)), "a", "b"),
      data.frame(a = c("2025-01-08", "2025-01-09", "2025-01-08"), b = c("2025-01-10", "2025-01-09", "2025-01-10"), c = c(1L, 2L, 3L))
    )
  })

test_that(
  "invert_columns_on_condition condition argument works correctly", {

    original <- data.frame(a = c(3L, 2L, 1L), b = c(1L, 2L, 3L), c = c(4L, 5L, 6L))
    expected <- data.frame(a = c(4L, 2L, 1L), b = c(1L, 2L, 3L), c = c(3L, 5L, 6L))
    condition <- original$c == 4L
    expect_equal(invert_columns_on_condition(original, "a", "c", condition), expected)
  })
