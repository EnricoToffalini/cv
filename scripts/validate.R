#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("R", "bibliography.R"))
source(file.path("R", "load_cv.R"))
source(file.path("R", "select_cv.R"))
source(file.path("R", "validate_cv.R"))

raw <- load_cv_raw()
result <- validate_cv_raw(raw)
output_errors <- validate_outputs(require_all = FALSE)
result$errors <- unique(c(result$errors, output_errors))
if (!report_validation(result)) quit(status = 1L)
