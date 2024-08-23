
# caption_latex ####

## helpers ####

### escape_percentage_sign_before_numbers_in_latex ####
test_that(
  "escape_percentage_sign_before_numbers_in_latex escapes % sign before numbers correctly",
  {
  expect_equal(escape_percentage_sign_before_numbers_in_latex("1%"), "1\\%")
  expect_equal(escape_percentage_sign_before_numbers_in_latex("10%"), "10\\%")
  expect_equal(escape_percentage_sign_before_numbers_in_latex("100%"), "100\\%")
  }
)
### escape_ampersand_sign_in_latex ####
test_that("escape_ampersand_sign_in_latex escapes & sign correctly", {
  expect_equal(escape_ampersand_sign_in_latex("done & dusted"), "done \\& dusted")
})
### unescape_star_sign_in_latex ####
test_that("unescape_star_sign_in_latex unescapes * sign correctly", {
  expect_equal(unescape_star_sign_in_latex("\\*"), "*")
})
### parse_references_in_latex ####
test_that("parse_references_in_latex parses references into latex format correctly", {
  expect_equal(parse_references_in_latex("\\@ref(slituR)"), "\\ref{slituR}")
})

## main ####
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


# caption_html ####

## helpers ####

### escape_percentage_sign_before_numbers_in_html ####
test_that("escape_percentage_sign_before_numbers_in_html unescapes % sign before numbers correctly", {
  expect_equal(escape_percentage_sign_before_numbers_in_html("1\\%"), "1%")
  expect_equal(escape_percentage_sign_before_numbers_in_html("10\\%"), "10%")
  expect_equal(escape_percentage_sign_before_numbers_in_html("100\\%"), "100%")
})
### escape_ampersand_sign_in_html ####
test_that("escape_ampersand_sign_in_html unescapes & sign correctly", {
  expect_equal(escape_ampersand_sign_in_html("done \\& dusted"), "done & dusted")
})
### unescape_star_sign_in_html ####
test_that("unescape_star_sign_in_html escapes * sign correctly", {
  expect_equal(unescape_star_sign_in_html("*"), "\\*")
})
### parse_references_in_html ####
test_that("parse_references_in_html parses references into latex format correctly", {
  expect_equal(parse_references_in_html("\\ref{slitu}"), "\\@ref(slitu)")
})

## main ####
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




# get_results ####
tables <- tibble::tibble(analysis = c('mtcars', 'iris'), results = list(mtcars, iris))

## helpers ####

### check_if_no_table_found ####
test_that("check_if_no_table_found prints message and returns null when specified analysis doesn't exists", {
  results <- tibble::tibble(); analysis <- "test"
  message <- paste0("No table found for analysis: ", analysis)
  expect_message(check_if_no_table_found(results, analysis), message)
  expect_equal(check_if_no_table_found(results, analysis), NULL)
})

### check_if_multiple_tables_found ####
test_that("check_if_multiple_tables_found prints message and returns null when specified analysis doesn't exists", {
  tables <- dplyr::bind_rows(tables, tables)
  analysis <- "iris"
  message <- paste0("Multiple tables found for analysis: ", analysis)
  expect_message(check_if_multiple_tables_found(tables, analysis), message)
  expect_equal(check_if_multiple_tables_found(tables, analysis), NULL)
})

## main ####
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


# style_table ####

## helpers ####

### style_latex_table ####
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

## main ####
test_that("style_table works as expected", {
  table <- style_table(type = "latex", table = kableExtra::kbl(mtcars, format = "latex"))
  expect_equal(attributes(table)$format, "latex")
  expect_equal(attributes(table)$lightable_class, "lightable-paper")
  table <- style_table(type = "html", table = kableExtra::kbl(mtcars, format = "html"))
  expect_equal(attributes(table)$format, "html")
  expect_equal(attributes(table)$lightable_class, "lightable-minimal")
})
