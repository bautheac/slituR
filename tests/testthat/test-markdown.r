
# caption_latex ################################################################

## helpers #####################################################################

### escape_percentage_sign_before_numbers_in_latex #############################
test_that(
  "escape_percentage_sign_before_numbers_in_latex escapes % sign before numbers correctly",
  {
  expect_equal(escape_percentage_sign_before_numbers_in_latex("1%"), "1\\%")
  expect_equal(escape_percentage_sign_before_numbers_in_latex("10%"), "10\\%")
  expect_equal(escape_percentage_sign_before_numbers_in_latex("100%"), "100\\%")
  }
)
### escape_ampersand_sign_in_latex #############################################
test_that("escape_ampersand_sign_in_latex escapes & sign correctly", {
  expect_equal(escape_ampersand_sign_in_latex("done & dusted"), "done \\& dusted")
})
### unescape_star_sign_in_latex ################################################
test_that("unescape_star_sign_in_latex unescapes * sign correctly", {
  expect_equal(unescape_star_sign_in_latex("\\*"), "*")
})
### parse_references_in_latex ##################################################
test_that("parse_references_in_latex parses references into latex format correctly", {
  expect_equal(parse_references_in_latex("\\@ref(slituR)"), "\\ref{slituR}")
})

## main ########################################################################
test_that(
  "parse_table_caption_for_latex_output escapes works correctly", {
  expect_equal(
    parse_table_caption_for_latex_output("1%, done & dusted, \\*, \\@ref(slituR)"),
    "1\\%, done \\& dusted, *, \\ref{slituR}"
  )
  expect_equal(
    caption_latex("1%, done & dusted, \\*, \\@ref(slituR)"),
    "1\\%, done \\& dusted, *, \\ref{slituR}"
  )
})


# caption_html #################################################################

## helpers #####################################################################

### escape_percentage_sign_before_numbers_in_html ##############################
test_that("escape_percentage_sign_before_numbers_in_html unescapes % sign before numbers correctly", {
  expect_equal(escape_percentage_sign_before_numbers_in_html("1\\%"), "1%")
  expect_equal(escape_percentage_sign_before_numbers_in_html("10\\%"), "10%")
  expect_equal(escape_percentage_sign_before_numbers_in_html("100\\%"), "100%")
})
### escape_ampersand_sign_in_html ##############################################
test_that("escape_ampersand_sign_in_html unescapes & sign correctly", {
  expect_equal(escape_ampersand_sign_in_html("done \\& dusted"), "done & dusted")
})
### unescape_star_sign_in_html #################################################
test_that("unescape_star_sign_in_html escapes * sign correctly", {
  expect_equal(unescape_star_sign_in_html("*"), "\\*")
})
### parse_references_in_html ###################################################
test_that("parse_references_in_html parses references into latex format correctly", {
  expect_equal(parse_references_in_html("\\ref{slitu}"), "\\@ref(slitu)")
})

## main ########################################################################
test_that(
  "parse_table_caption_for_html_output escapes works correctly", {
    expect_equal(
      parse_table_caption_for_html_output("1\\%, done \\& dusted, *, \\ref{slituR}"),
      "1%, done & dusted, \\*, \\@ref(slituR)"
    )
    expect_equal(
      caption_html("1\\%, done \\& dusted, *, \\ref{slituR}"),
      "1%, done & dusted, \\*, \\@ref(slituR)"
    )
  })


# get_results ##################################################################
tables <- tibble::tibble(analysis = c('mtcars', 'iris'), results = list(mtcars, iris))

## helpers #####################################################################

### check_if_no_table_found ####################################################
test_that("check_if_no_table_found prints message and returns null when specified analysis doesn't exists", {
  results <- tibble::tibble(); analysis <- "test"
  message <- paste0("No table found for analysis: ", analysis)
  expect_message(check_if_no_table_found(results, analysis), message)
  expect_equal(check_if_no_table_found(results, analysis), NULL)
})

### check_if_multiple_tables_found #############################################
test_that("check_if_multiple_tables_found prints message and returns null when specified analysis doesn't exists", {
  tables <- dplyr::bind_rows(tables, tables)
  analysis <- "iris"
  message <- paste0("Multiple tables found for analysis: ", analysis)
  expect_message(check_if_multiple_tables_found(tables, analysis), message)
  expect_equal(check_if_multiple_tables_found(tables, analysis), NULL)
})

## main ########################################################################
test_that("get_results retrieves existing results table correctly", {
  expect_equal(get_results(tables, 'mtcars'), mtcars)
  expect_equal(get_results(tables, 'iris'), iris)
})

test_that("get_results prints message and returns null when specified analysis doesn't exists", {
  analysis <- "USArrests"
  message <- paste0("No table found for analysis: ", analysis)
  expect_message(get_results(tables, analysis), message)
  expect_equal(get_results(tables, analysis), NULL)
})

test_that("get_results prints message and returns null when multiple tables found", {
  tables <- dplyr::bind_rows(tables, tables)
  analysis <- "iris"
  message <- paste0("Multiple tables found for analysis: ", analysis)
  expect_message(get_results(tables, analysis), message)
  expect_equal(get_results(tables, analysis), NULL)
})


# style_table ##################################################################

## helpers #####################################################################

### style_latex_table ##########################################################
test_that("style_latex_table paper styles latex table correctly", {
  table <- style_latex_table(table = kableExtra::kbl(mtcars, format = "latex"))
  expect_equal(attributes(table)$format, "latex")
  expect_equal(attributes(table)$lightable_class, "lightable-paper")
})
### style_html_table ####
test_that("style_html_table minimally styles html table correctly", {
  table <- style_html_table(table = kableExtra::kbl(mtcars, format = "html"))
  expect_equal(attributes(table)$format, "html")
  expect_equal(attributes(table)$lightable_class, "lightable-minimal")
})

## main ########################################################################
test_that("style_table works as expected", {
  table <- style_table(table = kableExtra::kbl(mtcars, format = "latex"), type = "latex")
  expect_equal(attributes(table)$format, "latex")
  expect_equal(attributes(table)$lightable_class, "lightable-paper")
  table <- style_table(table = kableExtra::kbl(mtcars, format = "html"), type = "html")
  expect_equal(attributes(table)$format, "html")
  expect_equal(attributes(table)$lightable_class, "lightable-minimal")
})


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


# collapse rows ################################################################

test_that("collapse_rows works as expected", {

  expect_true(exists("collapse_rows"))



  test_that("whenPassedOneColumnDataframeWithOneLevel_ReturnsDataframeWithCollapsedRows", {
    n <- 10L
    original <- tibble::tibble(collapse = rep("collapse", n))
    expected <- tibble::tibble(collapse = c("collapse", rep("", n - 1L)))
    expect_equal(collapse_rows(original), expected)
  })

  test_that("whenPassedOneColumnDataframeWithTwoLevels_ReturnsDataframeWithCollapsedRows", {
    n <- 5L
    original <- tibble::tibble(collapse = c(rep("collapse", n), rep("espalloc", n)))
    expected <- tibble::tibble(collapse = c(c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))))
    expect_equal(collapse_rows(original), expected)
  })

  test_that("whenPassedTwoColumnDataframeWithTwoLevels_ReturnsDataframeWithCollapsedRows", {
    n <- 5L
    original <- tibble::tibble(
      collapse = c(rep("collapse", n), rep("espalloc", n)),
      espalloc = c(rep("espalloc", n), rep("collapse", n))
      )
    expected <- tibble::tibble(
      collapse = c(c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))),
      espalloc = c(c("espalloc", rep("", n - 1L)), c("collapse", rep("", n - 1L)))
      )
    expect_equal(collapse_rows(original), expected)
  })

  test_that("whenPassedColumnNames_collapsesRowsOfCorrectColumns", {
    n <- 5L
    original <- tibble::tibble(
      collapse = c(rep("collapse", n), rep("espalloc", n)),
      espalloc = c(rep("espalloc", n), rep("collapse", n))
    )
    expected <- tibble::tibble(
      collapse = c(rep("collapse", n), rep("espalloc", n)),
      espalloc = c(c("espalloc", rep("", n - 1L)), c("collapse", rep("", n - 1L)))
    )
    expect_equal(collapse_rows(original, "espalloc"), expected)

    expected <- tibble::tibble(
      collapse = c(c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))),
      espalloc = c(rep("espalloc", n), rep("collapse", n))
    )
    expect_equal(collapse_rows(original, "collapse"), expected)

    expected <- tibble::tibble(
      collapse = c(c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))),
      espalloc = c(c("espalloc", rep("", n - 1L)), c("collapse", rep("", n - 1L)))
    )
    expect_equal(collapse_rows(original, c("collapse", "espalloc")), expected)
  })

  test_that("whenSequentialGroupsShowSameValue_RepeatsFirstRow", {
    n <- 5L
    original <- tibble::tibble(
      collapse = c(rep("collapse", n), rep("espalloc", n)),
      espalloc = c(rep("collapse", n + 3L), rep("espalloc", n - 3L))
    )
    expected <- tibble::tibble(
      collapse = c(c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))),
      espalloc = c(
        c("collapse", rep("", n - 1L)), c("collapse", rep("", 2L)), c("espalloc", "")
        )
    )
    expect_equal(collapse_rows(original, c("collapse", "espalloc")), expected)
  })

  test_that("whenPresentedwithAGroupOfOne_doesntCollapse", {
    n <- 5L
    original <- tibble::tibble(
      collapse = c("test", rep("collapse", n), rep("espalloc", n)),
      espalloc = c("test", rep("collapse", n + 3L), rep("espalloc", n - 3L))
    )
    expected <- tibble::tibble(
      collapse = c("test", c("collapse", rep("", n - 1L)), c("espalloc", rep("", n - 1L))),
      espalloc = c(
        "test", c("collapse", rep("", n - 1L)), c("collapse", rep("", 2L)), c("espalloc", "")
        )
    )
    expect_equal(collapse_rows(original, c("collapse", "espalloc")), expected)
  })
})
