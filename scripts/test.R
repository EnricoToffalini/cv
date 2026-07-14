#!/usr/bin/env Rscript

if (!requireNamespace("testthat", quietly = TRUE)) stop("Package 'testthat' is required", call. = FALSE)
result <- testthat::test_dir(file.path("tests", "testthat"), reporter = "summary", stop_on_failure = FALSE)
if (any(vapply(result, function(item) length(item$results) && any(vapply(item$results, inherits, logical(1), "expectation_failure")), logical(1)))) quit(status = 1L)
