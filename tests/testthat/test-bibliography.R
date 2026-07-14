testthat::test_that("a small BibTeX fixture renders", {
  publications <- read_publications(file.path(test_root, "tests", "fixtures", "sample.bib"))
  testthat::expect_equal(nrow(publications), 1)
  rendered <- format_publication(as.list(publications[1, ]))
  testthat::expect_match(rendered, "\\*\\*E[.] Toffalini\\*\\*")
  testthat::expect_match(rendered, "https://doi.org/10.1234/example")
})
