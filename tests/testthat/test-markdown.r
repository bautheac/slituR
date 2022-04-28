# caption_latex ####
test_that("caption_latex escapes % sign before numbers correctly", {
  expect_equal(caption_latex("1%"), "1\\%")
  expect_equal(caption_latex("10%"), "10\\%")
  expect_equal(caption_latex("100%"), "100\\%")
})

test_that("caption_latex escapes & sign correctly", {
  expect_equal(caption_latex("done & dusted"), "done \\& dusted")
})

test_that("caption_latex unescapes * sign correctly", {
  expect_equal(caption_latex("\\*"), "*")
})

test_that("caption_latex parses references into latex format correctly", {
  expect_equal(caption_latex("\\@ref(slitu)"), "\\ref{slitu}")
})






# caption_html ####
test_that("caption_html unescapes % sign before numbers correctly", {
  expect_equal(caption_html("1\\%"), "1%")
  expect_equal(caption_html("10\\%"), "10%")
  expect_equal(caption_html("100\\%"), "100%")
})

test_that("caption_html unescapes & sign correctly", {
  expect_equal(caption_html("done \\& dusted"), "done & dusted")
})

test_that("caption_html escapes * sign correctly", {
  expect_equal(caption_html("*"), "\\*")
})

test_that("caption_html parses references into latex format correctly", {
  expect_equal(caption_html("\\ref{slitu}"), "\\@ref(slitu)")
})


# get_results ####
tables <- tibble::tibble(analysis = c('mtcars', 'iris'), results = list(mtcars, iris))

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

test_that("get_results prints message and returns null when specified analysis doesn't exists", {
  tables <- dplyr::bind_rows(tables, tables)
  analysis <- "iris"
  message <- paste0("Multiple tables found for analysis: ", analysis)
  expect_message(get_results(tables, analysis), message)
  expect_equal(get_results(tables, analysis), NULL)
})

# style_table ####
test_that("style_table minimally styles html table", {
  table <- style_table(type = "html", table = kableExtra::kbl(mtcars, format = "html"))
  expect_equal(attributes(table)$format, "html")
  expect_equal(attributes(table)$lightable_class, "lightable-minimal")
})

test_that("style_table paper styles latex table", {
  table <- style_table(type = "latex", table = kableExtra::kbl(mtcars, format = "latex"))
  expect_equal(attributes(table)$format, "latex")
  expect_equal(attributes(table)$lightable_class, "lightable-paper")
})

