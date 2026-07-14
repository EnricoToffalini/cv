#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("R", "bibliography.R"))
source(file.path("R", "load_cv.R"))

rendered_path <- file.path("migration", "rendered-it-full.txt")
if (!file.exists(rendered_path)) stop("Missing rendered Italian full text: ", rendered_path, call. = FALSE)
rendered <- paste(readLines(rendered_path, warn = FALSE, encoding = "UTF-8"), collapse = " ")

normalize_text <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE)
}

haystack <- normalize_text(rendered)
raw <- load_cv_raw()
checks <- list()

add_checks <- function(collection, ids, values) {
  keep <- nzchar(values)
  checks[[length(checks) + 1L]] <<- data.frame(
    collection = collection,
    id = ids[keep],
    value = values[keep],
    present = vapply(values[keep], function(value) grepl(normalize_text(value), haystack, fixed = TRUE), logical(1)),
    stringsAsFactors = FALSE
  )
}

add_checks("positions", raw$positions$id, raw$positions$role_it)
add_checks("education", raw$education$id, raw$education$degree_it)
add_checks("grants", raw$grants$id, raw$grants$project_title_original)
add_checks("teaching", raw$teaching$id, raw$teaching$course_title_original)
add_checks("service", raw$service$id, raw$service$title_original)
add_checks("presentations", raw$presentations$id, raw$presentations$title_original)
add_checks("outreach", raw$outreach$id, raw$outreach$title_original)
add_checks("publications_title", raw$publications$citekey, raw$publications$title)
add_checks("publications_doi", raw$publications$citekey, raw$publications$doi)
add_checks("skills", vapply(raw$profile$skills, `[[`, character(1), "id"), vapply(raw$profile$skills, `[[`, character(1), "name_original"))

result <- do.call(rbind, checks)
write.csv(result, file.path("migration", "reconciliation_check.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
missing <- result[!result$present, , drop = FALSE]
if (nrow(missing)) {
  print(missing[, c("collection", "id", "value")], row.names = FALSE)
  stop(nrow(missing), " canonical values were not found in the Italian full PDF text", call. = FALSE)
}
message("Reconciliation check passed: ", nrow(result), " canonical values found in the Italian full PDF text.")
