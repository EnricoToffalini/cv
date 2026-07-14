testthat::test_that("partial ISO dates render in both languages", {
  testthat::expect_equal(format_cv_date("2025-10", "it", translations), "ottobre 2025")
  testthat::expect_equal(format_cv_date("2025-10", "en", translations), "October 2025")
  testthat::expect_equal(format_cv_date("2026-02-14", "it", translations), "14 febbraio 2026")
  testthat::expect_equal(format_cv_date("2026-02-14", "en", translations), "February 14, 2026")
})

testthat::test_that("current dates use the localized present label", {
  testthat::expect_equal(format_date_range("2025-10", "", TRUE, "it", translations), "ottobre 2025 – presente")
  testthat::expect_equal(format_date_range("2025-10", "", TRUE, "en", translations), "October 2025 – present")
})

testthat::test_that("language fallback follows primary, original, other", {
  row <- list(title_it = "", title_en = "English", title_original = "Original")
  testthat::expect_equal(localized_value(row, "title", "it", "title_original"), "Original")
  row$title_original <- ""
  testthat::expect_equal(localized_value(row, "title", "it", "title_original"), "English")
})

testthat::test_that("currency and DOI normalization are stable", {
  testthat::expect_equal(format_euro(224951, "it"), "€ 224.951")
  testthat::expect_equal(format_euro(224951, "en"), "€224,951")
  testthat::expect_equal(normalize_doi("https://doi.org/https://doi.org/10.3390/ABC."), "10.3390/abc")
})

testthat::test_that("author highlighting changes only Enrico Toffalini", {
  result <- format_person_list(c("E. Toffalini", "L. Toffalini", "C. Cornoldi"))
  testthat::expect_match(result, "\\*\\*E[.] Toffalini\\*\\*")
  testthat::expect_match(result, "L[.] Toffalini")
  testthat::expect_false(grepl("\\*\\*L[.] Toffalini", result))
})

testthat::test_that("records are sorted deterministically", {
  data <- data.frame(id = c("old", "new"), start_date = c("2020-01", "2025-01"), stringsAsFactors = FALSE)
  testthat::expect_equal(sort_records(data)$id, c("new", "old"))
})
