#' @export
modify_files_in_place_recursive <- function(
    root_directory, file_extension, modify_function, verbose = TRUE, ...
  ) {

  file_pattern <- paste0("\\.", file_extension, "$")
  file_paths <- list.files(
    root_directory, pattern = file_pattern, full.names = TRUE, recursive = TRUE
  )

  purrr::walk(file_paths, function(file_path) {

    data <- readRDS(file_path)

    modified_data <- modify_function(data, ...)

    saveRDS(modified_data, file_path)

    if (verbose) message("Processed: ", file_path)
  })
}


#' @export
extract_packages_from_files <- function(directory) {
  # Get all R files recursively (including modules)
  r_files <- list.files(directory, pattern = "\\.r$", full.names = TRUE, recursive = TRUE)

  # Function to extract package names from a file
  extract_packages <- function(file) {
    lines <- readLines(file, warn = FALSE)

    # Look for library(), require(), and import()
    matches <- unlist(
      regmatches(
        lines,
        gregexpr(
          "(?<=library\\(|require\\(|import\\()[a-zA-Z0-9.]+", lines, perl = TRUE
        )
      )
    )

    return(matches)
  }

  # Apply extraction function to all R files
  packages <- unique(unlist(lapply(r_files, extract_packages)))

  # Remove base R functions mistakenly captured
  base_packages <- rownames(installed.packages(priority = "base"))
  packages <- setdiff(packages, base_packages)

  return(packages)
}
