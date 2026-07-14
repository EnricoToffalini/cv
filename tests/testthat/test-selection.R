testthat::test_that("selection modes select, preserve, summarize, or omit", {
  data <- data.frame(id = c("a", "b"), include_short = c(TRUE, FALSE), sort_order = c(1, 2))
  testthat::expect_equal(select_by_mode(data, "all")$id, c("a", "b"))
  testthat::expect_equal(select_by_mode(data, "selected")$id, "a")
  testthat::expect_equal(nrow(select_by_mode(data, "omit")), 0)
  testthat::expect_equal(nrow(select_by_mode(data, "summary")), 2)
})

testthat::test_that("short CV is a subset of full CV", {
  raw <- load_cv_raw(test_root)
  full <- select_cv(raw, "it", "full")
  short <- select_cv(raw, "it", "short")
  for (section in raw$variants$section_order) {
    full_data <- full$sections[[section]]$data
    short_data <- short$sections[[section]]$data
    if (is.data.frame(full_data) && is.data.frame(short_data)) {
      key <- if ("id" %in% names(full_data)) "id" else if ("citekey" %in% names(full_data)) "citekey" else NULL
      if (!is.null(key)) testthat::expect_true(all(short_data[[key]] %in% full_data[[key]]), info = section)
    }
  }
})

testthat::test_that("canonical ids and DOIs are unique", {
  raw <- load_cv_raw(test_root)
  result <- validate_cv_raw(raw, root = test_root)
  testthat::expect_length(result$errors, 0)
})
